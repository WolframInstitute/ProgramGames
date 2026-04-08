//! CPU tournament engine for iterated TM games.
//!
//! Each pair of TMs plays an iterated game for `rounds` rounds.
//! Both TMs see the SAME flattened history as their tape input.
//! History is maintained as a bit array: [a1, b1, a2, b2, ...].
//!
//! Also supports mixed-strategy tournaments with FSM and CA strategies.

use rayon::prelude::*;
use serde::Serialize;
use std::io::{self, BufRead, BufWriter, Write};
use std::path::PathBuf;

use crate::strategy::{play_game_dyn, play_game_with_history, StrategyRunner, StrategySpec};
use crate::TmTransition;

// ── History-to-integer conversion (matches WL HistoryToInput/FromDigits) ────

/// Run a TM with history as input, matching WL HistoryToInput + OneSidedTuringMachineFunction.
///
/// WL does: FromDigits[Flatten[history], base] → integer → IntegerDigits → tape.
/// The FromDigits→IntegerDigits roundtrip is equivalent to stripping leading zeros.
/// This avoids integer overflow for long histories (200+ binary digits > u64 range).
fn run_tm_from_history(
    transitions: &[TmTransition],
    symbols: u8,
    history: &[u8],
    max_steps: u32,
) -> (bool, u8) {
    // Strip leading zeros (equivalent to FromDigits → IntegerDigits roundtrip)
    let start = history.iter().position(|&d| d != 0).unwrap_or(history.len());
    let digits = if start >= history.len() {
        // All zeros or empty → input is 0 → tape is [0]
        vec![0u8]
    } else {
        history[start..].to_vec()
    };

    // Run TM with digits as tape (same as run_tm but from digits directly)
    let k = symbols.max(2);
    let mut tape = digits;
    let mut head: usize = tape.len().saturating_sub(1);
    let mut state: u16 = 1;

    for _step in 0..max_steps {
        let read = tape.get(head).copied().unwrap_or(0);
        let idx = (state.saturating_sub(1) as usize) * (k as usize) + (read as usize);
        let Some(&trans) = transitions.get(idx) else {
            return (false, 0);
        };
        if let Some(cell) = tape.get_mut(head) {
            *cell = trans.write;
        }
        if trans.move_right && head + 1 == tape.len() {
            let out = tape.last().copied().unwrap_or(0);
            return (true, out % 2);
        }
        if trans.move_right {
            if head + 1 < tape.len() { head += 1; }
        } else if head == 0 {
            tape.insert(0, 0);
        } else {
            head -= 1;
        }
        state = trans.next;
        if state == 0 { return (false, 0); }
    }
    (false, 0)
}

// ── Payoff parsing ──────────────────────────────────────────────────────────

/// Payoff matrix: payoff[move_a * 2 + move_b] = [score_a, score_b]
pub type Payoff = [[i32; 2]; 4];

/// Parse a game name or custom payoff string into a Payoff matrix.
/// Index: CC=0, CD=1, DC=2, DD=3 (move_a * 2 + move_b, 0=cooperate, 1=defect)
pub fn parse_game(game: &str) -> Result<Payoff, String> {
    match game.trim().to_lowercase().as_str() {
        "pd" => Ok([
            [-1, -1],  // CC
            [-3, 0],   // CD
            [0, -3],   // DC
            [-2, -2],  // DD
        ]),
        "chicken" => Ok([
            [0, 0],      // CC
            [-1, 1],     // CD
            [1, -1],     // DC
            [-10, -10],  // DD
        ]),
        custom => {
            let vals: Vec<i32> = custom
                .split(',')
                .map(|s| s.trim().parse::<i32>().map_err(|e| format!("bad payoff '{}': {}", s.trim(), e)))
                .collect::<Result<Vec<_>, _>>()?;
            if vals.len() != 8 {
                return Err(format!(
                    "custom payoff needs 8 values (CC_a,CC_b,CD_a,CD_b,DC_a,DC_b,DD_a,DD_b), got {}",
                    vals.len()
                ));
            }
            Ok([
                [vals[0], vals[1]],
                [vals[2], vals[3]],
                [vals[4], vals[5]],
                [vals[6], vals[7]],
            ])
        }
    }
}

// ── Dynamic (k-action) payoff ────────────────────────────────────────────────

/// Dynamic payoff matrix for k-action games.
/// entries[move_a * num_actions + move_b] = [score_a, score_b]
pub struct DynPayoff {
    pub num_actions: usize,
    pub entries: Vec<[i32; 2]>,
}

impl DynPayoff {
    pub fn get(&self, idx: usize) -> Option<&[i32; 2]> {
        self.entries.get(idx)
    }
}

/// Parse a game string into a DynPayoff supporting any number of actions.
/// Named games: "pd" (2-action), "chicken" (2-action).
/// Custom: comma-separated values, count must be 2*k² for some k >= 2.
pub fn parse_game_dyn(game: &str) -> Result<DynPayoff, String> {
    // Try named games first
    match game.trim().to_lowercase().as_str() {
        "pd" | "prisoners_dilemma" => {
            return Ok(DynPayoff {
                num_actions: 2,
                entries: vec![[-1, -1], [-3, 0], [0, -3], [-2, -2]],
            });
        }
        "chicken" => {
            return Ok(DynPayoff {
                num_actions: 2,
                entries: vec![[0, 0], [-1, 1], [1, -1], [-10, -10]],
            });
        }
        _ => {}
    }

    // Parse custom: comma-separated values
    let vals: Vec<i32> = game
        .split(',')
        .map(|s| s.trim().parse::<i32>())
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| format!("bad payoff value: {}", e))?;

    if vals.len() < 8 || vals.len() % 2 != 0 {
        return Err(format!("payoff needs 2*k² values (got {})", vals.len()));
    }

    let num_outcomes = vals.len() / 2;
    let num_actions = (num_outcomes as f64).sqrt() as usize;
    if num_actions * num_actions != num_outcomes {
        return Err(format!(
            "payoff count {} is not 2*k² for any k",
            vals.len()
        ));
    }

    let entries: Vec<[i32; 2]> = vals.chunks(2).map(|c| [c[0], c[1]]).collect();
    Ok(DynPayoff {
        num_actions,
        entries,
    })
}

// ── TM ID parsing ───────────────────────────────────────────────────────────

pub fn parse_ids(ids: &Option<String>, ids_file: &Option<PathBuf>) -> Result<Vec<u64>, String> {
    if let Some(inline) = ids {
        return inline
            .split(',')
            .map(|s| {
                s.trim()
                    .parse::<u64>()
                    .map_err(|e| format!("bad TM id '{}': {}", s.trim(), e))
            })
            .collect();
    }
    if let Some(path) = ids_file {
        let reader: Box<dyn BufRead> = if path.to_string_lossy() == "-" {
            Box::new(io::stdin().lock())
        } else {
            let f = std::fs::File::open(path)
                .map_err(|e| format!("cannot open {}: {}", path.display(), e))?;
            Box::new(io::BufReader::new(f))
        };
        let mut ids = Vec::new();
        for line in reader.lines() {
            let line = line.map_err(|e| format!("read error: {}", e))?;
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with('#') {
                continue;
            }
            ids.push(
                trimmed
                    .parse::<u64>()
                    .map_err(|e| format!("bad TM id '{}': {}", trimmed, e))?,
            );
        }
        return Ok(ids);
    }
    Err("must provide --ids or --ids-file (use '-' for stdin)".into())
}

// ── Tape-based TM runner ────────────────────────────────────────────────────

/// Run a TM on a pre-built tape (history bits, MSD-first).
/// Returns (halted, output_mod2).
///
/// Tape layout: [0 0 ... 0 | history[0] history[1] ... history[n-1]]
///               ^left_pad   ^-- tape data
///
/// Head starts at rightmost history bit.
/// Halts when moving right past the last position.
/// Returns 0 (cooperate) on timeout / no-halt.
pub fn run_tm_on_tape(
    transitions: &[TmTransition],
    symbols: u8,
    tape_data: &[u8],
    max_steps: u32,
) -> (bool, u8) {
    let left_pad = max_steps as usize + 1;
    let tape_len = left_pad + tape_data.len();
    let mut buf = vec![0u8; tape_len];
    buf[left_pad..].copy_from_slice(tape_data);

    let mut head = if tape_data.is_empty() {
        // Empty tape: head at the single blank position
        left_pad.saturating_sub(1)
    } else {
        tape_len - 1
    };
    let mut state: u16 = 1;
    let k = symbols as usize;

    for _ in 0..max_steps {
        let read = buf[head] as usize;
        let idx = (state as usize - 1) * k + read;
        if idx >= transitions.len() {
            return (false, 0);
        }
        let trans = transitions[idx];
        buf[head] = trans.write;

        if trans.move_right {
            if head + 1 == tape_len {
                return (true, buf[tape_len - 1] % 2);
            }
            head += 1;
        } else {
            if head == 0 {
                return (false, 0); // ran off left edge
            }
            head -= 1;
        }

        state = trans.next;
        if state == 0 {
            return (false, 0);
        }
    }
    (false, 0) // timeout
}

// ── CPU game play ───────────────────────────────────────────────────────────

/// Play one iterated game between two TMs.
/// Returns ((score_a, score_b), failed_flag).
/// failed_flag: 0 = both halted, 1 = a failed, 2 = b failed, 3 = both failed.
pub fn play_game_cpu(
    trans_a: &[TmTransition],
    trans_b: &[TmTransition],
    symbols: u8,
    max_steps: u32,
    rounds: u32,
    payoff: &DynPayoff,
) -> ((i64, i64), u8) {
    let na = payoff.num_actions;
    let mut history: Vec<u8> = Vec::with_capacity(2 * rounds as usize);
    let mut score_a = 0i64;
    let mut score_b = 0i64;
    let mut failed: u8 = 0;

    for round in 0..rounds {
        let (move_a, move_b) = if round == 0 {
            (0u8, 0u8) // cooperate on empty history
        } else {
            // Run TMs with history as input (strips leading zeros to match WL FromDigits)
            let (ha, out_a) = run_tm_from_history(trans_a, symbols, &history, max_steps);
            let (hb, out_b) = run_tm_from_history(trans_b, symbols, &history, max_steps);
            if !ha { failed |= 1; }
            if !hb { failed |= 2; }
            let ma = if ha { out_a % na as u8 } else { 1 }; // timeout -> defect
            let mb = if hb { out_b % na as u8 } else { 1 };
            (ma, mb)
        };

        history.push(move_a);
        history.push(move_b);

        let idx = (move_a as usize) * na + move_b as usize;
        if let Some(payoffs) = payoff.get(idx) {
            score_a += payoffs[0] as i64;
            score_b += payoffs[1] as i64;
        }
    }

    ((score_a, score_b), failed)
}

// ── CPU tournament ──────────────────────────────────────────────────────────

/// Build all ordered pairs (i, j) including self-play (Tuples[ids, 2] in WL).
pub fn all_pairs(n: usize) -> Vec<(usize, usize)> {
    // Include self-play to match WL Tuples[list, 2] convention
    let mut pairs = Vec::with_capacity(n * n);
    for i in 0..n {
        for j in 0..n {
            pairs.push((i, j));
        }
    }
    pairs
}

/// Run the full tournament on CPU using rayon.
/// Returns (survivors, a_scores, b_scores) where survivors are indices of TMs that
/// always halted, and score matrices only include survivors.
/// a_scores[i][j] = player i's payoff as A when i plays A against j as B.
/// b_scores[i][j] = player j's payoff as B when i plays A against j as B.
pub fn run_tournament_cpu(
    ids: &[u64],
    states: u16,
    symbols: u8,
    max_steps: u32,
    rounds: u32,
    payoff: &DynPayoff,
) -> (Vec<usize>, Vec<Vec<i64>>, Vec<Vec<i64>>) {
    let n = ids.len();
    let transitions: Vec<Vec<TmTransition>> = ids
        .iter()
        .map(|&id| crate::decode_tm(id, states, symbols))
        .collect();

    let pairs = all_pairs(n);

    eprintln!(
        "  CPU tournament: {} machines, {} pairs, {} rounds",
        n,
        pairs.len(),
        rounds
    );

    // Parallel over pairs, tracking non-halting TMs
    use std::sync::Mutex;
    let failed_set: Mutex<std::collections::HashSet<usize>> =
        Mutex::new(std::collections::HashSet::new());

    let results: Vec<((usize, usize), (i64, i64))> = pairs
        .par_iter()
        .map(|&(i, j)| {
            let ((sa, sb), failed_flag) = play_game_cpu(
                &transitions[i],
                &transitions[j],
                symbols,
                max_steps,
                rounds,
                payoff,
            );
            if failed_flag != 0 {
                let mut fs = failed_set.lock().unwrap();
                if failed_flag & 1 != 0 { fs.insert(i); }
                if failed_flag & 2 != 0 { fs.insert(j); }
            }
            ((i, j), (sa, sb))
        })
        .collect();

    let failed = failed_set.into_inner().unwrap();
    if !failed.is_empty() {
        let failed_ids: Vec<u64> = failed.iter().map(|&i| ids[i]).collect();
        eprintln!("  Excluded {} non-halting TMs: {:?}", failed.len(), failed_ids);
    }

    // Build list of surviving indices
    let survivors: Vec<usize> = (0..n).filter(|i| !failed.contains(i)).collect();
    let m = survivors.len();

    // Build compact score matrices with only survivors
    let mut a_matrix = vec![vec![0i64; m]; m];
    let mut b_matrix = vec![vec![0i64; m]; m];
    for ((i, j), (sa, sb)) in &results {
        if !failed.contains(i) && !failed.contains(j) {
            let si = survivors.iter().position(|&x| x == *i).unwrap();
            let sj = survivors.iter().position(|&x| x == *j).unwrap();
            a_matrix[si][sj] = *sa;
            b_matrix[si][sj] = *sb;
        }
    }

    (survivors, a_matrix, b_matrix)
}

// ── Output ──────────────────────────────────────────────────────────────────

#[derive(Serialize)]
pub struct TournamentOutput {
    pub ids: Vec<u64>,
    pub rounds: u32,
    pub game: String,
    pub states: u16,
    pub symbols: u8,
    pub num_machines: usize,
    pub num_pairs: usize,
    pub scores: Vec<Vec<i64>>,
    pub b_scores: Vec<Vec<i64>>,
    pub pairwise: Vec<PairResult>,
    pub ranking: Vec<RankEntry>,
}

#[derive(Serialize)]
pub struct PairResult {
    pub i: usize,
    pub j: usize,
    pub id_a: u64,
    pub id_b: u64,
    pub score_a: f64,
    pub score_b: f64,
}

#[derive(Serialize)]
pub struct RankEntry {
    pub id: u64,
    pub total: i64,
    pub mean: f64,
    pub median: f64,
    pub wins: usize,
    pub losses: usize,
    pub draws: usize,
}

pub fn build_output(
    ids: &[u64],
    a_scores: Vec<Vec<i64>>,
    b_scores: Vec<Vec<i64>>,
    rounds: u32,
    game: &str,
    states: u16,
    symbols: u8,
) -> TournamentOutput {
    let n = ids.len();

    // Pairwise results (all ordered pairs including self-play)
    // Mean-aggregated over rounds to match WL TournamentScores convention.
    let r = rounds as f64;
    let mut pairwise = Vec::with_capacity(n * n);
    for i in 0..n {
        for j in 0..n {
            pairwise.push(PairResult {
                i,
                j,
                id_a: ids[i],
                id_b: ids[j],
                score_a: a_scores[i][j] as f64 / r,
                score_b: b_scores[i][j] as f64 / r,
            });
        }
    }

    // Ranking: match WL Code-02.nb TournamentScoreboard convention.
    // Both perspectives, Mean-aggregated over rounds, 2*n games per player.
    // a_scores[i][j] = player i's payoff as A when i plays A against j as B
    // b_scores[i][j] = player j's payoff as B when i plays A against j as B
    let mut ranking: Vec<RankEntry> = ids
        .iter()
        .enumerate()
        .map(|(i, &id)| {
            let mut wins = 0usize;
            let mut losses = 0usize;
            let mut draws = 0usize;
            let mut total_payoff = 0.0f64;

            for j in 0..n {
                // Pair (i, j): i is player A, j is player B
                let pay_i_as_a = a_scores[i][j] as f64 / r;
                let pay_j_as_b = b_scores[i][j] as f64 / r;
                let margin = pay_i_as_a - pay_j_as_b;
                total_payoff += pay_i_as_a;
                if margin > 0.0 { wins += 1; }
                else if margin < 0.0 { losses += 1; }
                else { draws += 1; }

                // Pair (j, i): i is player B, j is player A
                // i's payoff as B = b_scores[j][i] (j plays A, i plays B)
                // j's payoff as A = a_scores[j][i]
                let pay_i_as_b = b_scores[j][i] as f64 / r;
                let pay_j_as_a = a_scores[j][i] as f64 / r;
                let margin_b = pay_i_as_b - pay_j_as_a;
                total_payoff += pay_i_as_b;
                if margin_b > 0.0 { wins += 1; }
                else if margin_b < 0.0 { losses += 1; }
                else { draws += 1; }
            }

            let games = 2 * n;
            let mean = total_payoff / games as f64;
            // For median: collect per-game payoffs from all games
            let mut per_game: Vec<f64> = Vec::with_capacity(games);
            for j in 0..n {
                per_game.push(a_scores[i][j] as f64 / r); // as player A in (i, j)
                per_game.push(b_scores[j][i] as f64 / r); // as player B in (j, i)
            }
            per_game.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
            let median = if per_game.is_empty() {
                0.0
            } else if per_game.len() % 2 == 0 {
                (per_game[per_game.len() / 2 - 1] + per_game[per_game.len() / 2]) / 2.0
            } else {
                per_game[per_game.len() / 2]
            };

            // raw_total = sum of payoffs as A + sum of payoffs as B
            let raw_total: i64 = (0..n).map(|j| a_scores[i][j] + b_scores[j][i]).sum();
            RankEntry { id, total: raw_total, mean, median, wins, losses, draws }
        })
        .collect();
    ranking.sort_by(|a, b| b.mean.partial_cmp(&a.mean).unwrap_or(std::cmp::Ordering::Equal));

    TournamentOutput {
        ids: ids.to_vec(),
        rounds,
        game: game.to_string(),
        states,
        symbols,
        num_machines: n,
        num_pairs: n * n,
        scores: a_scores,
        b_scores,
        pairwise,
        ranking,
    }
}



pub fn write_output(output: &TournamentOutput) {
    let stdout = io::stdout();
    let mut out = BufWriter::new(stdout.lock());
    serde_json::to_writer_pretty(&mut out, output).ok();
    out.write_all(b"\n").ok();
    out.flush().ok();
}

// ── Strategy spec parsing ────────────────────────────────────────────────────

/// Parse strategy specs from an NDJSON file (one JSON object per line).
/// Use path "-" for stdin.
pub fn parse_strategies_file(path: &PathBuf) -> Result<Vec<StrategySpec>, String> {
    let reader: Box<dyn BufRead> = if path.to_string_lossy() == "-" {
        Box::new(io::stdin().lock())
    } else {
        let f = std::fs::File::open(path)
            .map_err(|e| format!("cannot open {}: {}", path.display(), e))?;
        Box::new(io::BufReader::new(f))
    };
    let mut specs = Vec::new();
    for (line_num, line) in reader.lines().enumerate() {
        let line = line.map_err(|e| format!("read error: {}", e))?;
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        let spec: StrategySpec = serde_json::from_str(trimmed)
            .map_err(|e| format!("line {}: bad strategy spec '{}': {}", line_num + 1, trimmed, e))?;
        specs.push(spec);
    }
    Ok(specs)
}

/// Parse strategy specs from an inline NDJSON string.
pub fn parse_strategies_inline(ndjson: &str) -> Result<Vec<StrategySpec>, String> {
    let mut specs = Vec::new();
    for (line_num, line) in ndjson.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        let spec: StrategySpec = serde_json::from_str(trimmed)
            .map_err(|e| format!("line {}: bad strategy spec '{}': {}", line_num + 1, trimmed, e))?;
        specs.push(spec);
    }
    Ok(specs)
}

// ── Mixed-strategy tournament ────────────────────────────────────────────────

/// Output for mixed-strategy tournaments.
#[derive(Serialize)]
pub struct MixedTournamentOutput {
    pub strategies: Vec<String>,
    pub rounds: u32,
    pub game: String,
    pub num_strategies: usize,
    pub num_pairs: usize,
    pub scores: Vec<Vec<i64>>,
    pub b_scores: Vec<Vec<i64>>,
    pub pairwise: Vec<MixedPairResult>,
    pub ranking: Vec<MixedRankEntry>,
}

#[derive(Serialize)]
pub struct MixedPairResult {
    pub i: usize,
    pub j: usize,
    pub label_a: String,
    pub label_b: String,
    pub score_a: f64,
    pub score_b: f64,
}

#[derive(Serialize)]
pub struct MixedRankEntry {
    pub label: String,
    pub total: i64,
    pub mean: f64,
    pub median: f64,
    pub wins: usize,
    pub losses: usize,
    pub draws: usize,
}

/// Run a mixed-strategy tournament on CPU using rayon.
/// Returns (survivor_indices, a_scores, b_scores) where the matrices only contain
/// strategies that halted on every input. Non-halting strategies are excluded.
/// a_scores[i][j] = player i's payoff as A when i plays A against j as B.
/// b_scores[i][j] = player j's payoff as B when i plays A against j as B.
pub fn run_mixed_tournament_cpu(
    specs: &[StrategySpec],
    rounds: u32,
    payoff: &DynPayoff,
) -> (Vec<usize>, Vec<Vec<i64>>, Vec<Vec<i64>>) {
    // Set num_actions on each spec from the payoff
    let mut specs = specs.to_vec();
    for spec in &mut specs {
        match spec {
            StrategySpec::Ca { num_actions, .. }
            | StrategySpec::Tm { num_actions, .. }
            | StrategySpec::Fsm { num_actions, .. }
            | StrategySpec::RuleArray { num_actions, .. } => {
                *num_actions = payoff.num_actions as u8;
            }
        }
    }

    let n = specs.len();
    let pairs = all_pairs(n);

    eprintln!(
        "  CPU mixed tournament: {} strategies, {} pairs, {} rounds",
        n,
        pairs.len(),
        rounds
    );

    // Parallel over pairs. Each thread creates its own runners.
    // Collect which strategies failed to halt.
    use std::sync::Mutex;
    let failed_set: Mutex<std::collections::HashSet<usize>> = Mutex::new(std::collections::HashSet::new());

    let results: Vec<((usize, usize), Option<(i64, i64)>)> = pairs
        .par_iter()
        .map(|&(i, j)| {
            // Skip if either player already known to be failed
            {
                let fs = failed_set.lock().unwrap();
                if fs.contains(&i) || fs.contains(&j) {
                    return ((i, j), None);
                }
            }
            let mut runner_a = StrategyRunner::new(&specs[i]);
            let mut runner_b = StrategyRunner::new(&specs[j]);
            let (result, failed_flag) = play_game_dyn(&mut runner_a, &mut runner_b, rounds, payoff);
            if failed_flag != 0 {
                let mut fs = failed_set.lock().unwrap();
                if failed_flag & 1 != 0 { fs.insert(i); }
                if failed_flag & 2 != 0 { fs.insert(j); }
            }
            ((i, j), result)
        })
        .collect();

    let failed = failed_set.into_inner().unwrap();
    if !failed.is_empty() {
        let failed_labels: Vec<String> = failed.iter()
            .map(|&i| specs[i].label())
            .collect();
        eprintln!("  Excluded {} non-halting strategies: {:?}", failed.len(), failed_labels);
    }

    // Build list of surviving indices
    let survivors: Vec<usize> = (0..n).filter(|i| !failed.contains(i)).collect();
    let m = survivors.len();

    // Build compact score matrices with only survivors
    let mut a_matrix = vec![vec![0i64; m]; m];
    let mut b_matrix = vec![vec![0i64; m]; m];
    for ((i, j), result) in &results {
        if let Some((sa, sb)) = result {
            if !failed.contains(i) && !failed.contains(j) {
                let si = survivors.iter().position(|&x| x == *i).unwrap();
                let sj = survivors.iter().position(|&x| x == *j).unwrap();
                a_matrix[si][sj] = *sa;
                b_matrix[si][sj] = *sb;
            }
        }
    }

    // Return only surviving specs and the compact matrices
    (survivors, a_matrix, b_matrix)
}

/// Build output for a mixed-strategy tournament.
/// a_scores[i][j] = player i's payoff as A when i plays A against j as B.
/// b_scores[i][j] = player j's payoff as B when i plays A against j as B.
pub fn build_mixed_output(
    specs: &[StrategySpec],
    a_scores: Vec<Vec<i64>>,
    b_scores: Vec<Vec<i64>>,
    rounds: u32,
    game: &str,
) -> MixedTournamentOutput {
    let n = specs.len();
    let labels: Vec<String> = specs.iter().map(|s| s.label()).collect();

    // Pairwise results (all ordered pairs including self-play)
    // Mean-aggregated over rounds to match WL TournamentScores convention.
    let r = rounds as f64;
    let mut pairwise = Vec::with_capacity(n * n);
    for i in 0..n {
        for j in 0..n {
            pairwise.push(MixedPairResult {
                i,
                j,
                label_a: labels[i].clone(),
                label_b: labels[j].clone(),
                score_a: a_scores[i][j] as f64 / r,
                score_b: b_scores[i][j] as f64 / r,
            });
        }
    }

    // Ranking: match WL Code-02.nb TournamentScoreboard convention.
    // a_scores[i][j] = player i's payoff as A when i plays A against j as B
    // b_scores[i][j] = player j's payoff as B when i plays A against j as B
    // Win/loss is determined by Sign(payoff_A - payoff_B) per pair.
    // Each strategy has 2*n games (n as player A, n as player B including self-play).
    let mut ranking: Vec<MixedRankEntry> = labels
        .iter()
        .enumerate()
        .map(|(i, label)| {
            let mut wins = 0usize;
            let mut losses = 0usize;
            let mut draws = 0usize;
            let mut total_payoff = 0.0f64;

            for j in 0..n {
                // Pair (i, j): i is player A, j is player B
                let pay_i_as_a = a_scores[i][j] as f64 / r;
                let pay_j_as_b = b_scores[i][j] as f64 / r;
                let margin = pay_i_as_a - pay_j_as_b;
                total_payoff += pay_i_as_a;
                if margin > 0.0 { wins += 1; }
                else if margin < 0.0 { losses += 1; }
                else { draws += 1; }

                // Pair (j, i): i is player B, j is player A
                // i's payoff as B = b_scores[j][i] (j plays A, i plays B)
                // j's payoff as A = a_scores[j][i]
                let pay_i_as_b = b_scores[j][i] as f64 / r;
                let pay_j_as_a = a_scores[j][i] as f64 / r;
                let margin_b = pay_i_as_b - pay_j_as_a;
                total_payoff += pay_i_as_b;
                if margin_b > 0.0 { wins += 1; }
                else if margin_b < 0.0 { losses += 1; }
                else { draws += 1; }
            }

            let games = 2 * n;
            let mean = total_payoff / games as f64;
            // For median: collect per-game payoffs from all games
            let mut per_game: Vec<f64> = Vec::with_capacity(games);
            for j in 0..n {
                per_game.push(a_scores[i][j] as f64 / r); // as player A in (i, j)
                per_game.push(b_scores[j][i] as f64 / r); // as player B in (j, i)
            }
            per_game.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
            let median = if per_game.is_empty() {
                0.0
            } else if per_game.len() % 2 == 0 {
                (per_game[per_game.len() / 2 - 1] + per_game[per_game.len() / 2]) / 2.0
            } else {
                per_game[per_game.len() / 2]
            };

            // raw_total = sum of payoffs as A + sum of payoffs as B
            let raw_total: i64 = (0..n).map(|j| a_scores[i][j] + b_scores[j][i]).sum();
            MixedRankEntry {
                label: label.clone(),
                total: raw_total,
                mean,
                median,
                wins,
                losses,
                draws,
            }
        })
        .collect();
    ranking.sort_by(|a, b| b.mean.partial_cmp(&a.mean).unwrap_or(std::cmp::Ordering::Equal));

    MixedTournamentOutput {
        strategies: labels,
        rounds,
        game: game.to_string(),
        num_strategies: n,
        num_pairs: n * n,
        scores: a_scores,
        b_scores,
        pairwise,
        ranking,
    }
}

// ── Iterated game (history-returning) ───────────────────────────────────────

/// Output for a single iterated game between two strategies.
#[derive(Serialize)]
pub struct IteratedGameOutput {
    pub label_a: String,
    pub label_b: String,
    pub history: Vec<[u8; 2]>,
    pub rounds: u32,
    pub failed: u8,
}

/// Output for an iterated game tournament (all pairs, with histories).
#[derive(Serialize)]
pub struct IteratedGameTournamentOutput {
    pub strategies: Vec<String>,
    pub rounds: u32,
    pub num_strategies: usize,
    pub num_pairs: usize,
    pub games: Vec<IteratedGamePairOutput>,
    pub failed: Vec<usize>,
}

#[derive(Serialize)]
pub struct IteratedGamePairOutput {
    pub i: usize,
    pub j: usize,
    pub label_a: String,
    pub label_b: String,
    pub history: Vec<[u8; 2]>,
}

/// Run an iterated game tournament on CPU, returning full move histories for all pairs.
/// Returns (survivor_indices, failed_indices, game_outputs).
/// Games involving non-halting strategies get empty histories.
pub fn run_iterated_game_tournament(
    specs: &[StrategySpec],
    rounds: u32,
    initial_history: &[[u8; 2]],
) -> (Vec<usize>, Vec<usize>, Vec<IteratedGamePairOutput>) {
    let n = specs.len();
    let pairs = all_pairs(n);

    eprintln!(
        "  CPU iterated game tournament: {} strategies, {} pairs, {} rounds (init_len={})",
        n,
        pairs.len(),
        rounds,
        initial_history.len()
    );

    use std::sync::Mutex;
    let failed_set: Mutex<std::collections::HashSet<usize>> =
        Mutex::new(std::collections::HashSet::new());

    let results: Vec<((usize, usize), Vec<[u8; 2]>)> = pairs
        .par_iter()
        .map(|&(i, j)| {
            {
                let fs = failed_set.lock().unwrap();
                if fs.contains(&i) || fs.contains(&j) {
                    return ((i, j), vec![]);
                }
            }
            let mut runner_a = StrategyRunner::new(&specs[i]);
            let mut runner_b = StrategyRunner::new(&specs[j]);
            let (history, failed_flag) =
                play_game_with_history(&mut runner_a, &mut runner_b, rounds, initial_history);
            if failed_flag != 0 {
                let mut fs = failed_set.lock().unwrap();
                if failed_flag & 1 != 0 {
                    fs.insert(i);
                }
                if failed_flag & 2 != 0 {
                    fs.insert(j);
                }
            }
            ((i, j), history)
        })
        .collect();

    let failed = failed_set.into_inner().unwrap();
    if !failed.is_empty() {
        let failed_labels: Vec<String> = failed.iter().map(|&i| specs[i].label()).collect();
        eprintln!(
            "  {} non-halting strategies: {:?}",
            failed.len(),
            failed_labels
        );
    }

    let survivors: Vec<usize> = (0..n).filter(|i| !failed.contains(i)).collect();
    let mut failed_sorted: Vec<usize> = failed.iter().copied().collect();
    failed_sorted.sort();
    let labels: Vec<String> = specs.iter().map(|s| s.label()).collect();

    let games: Vec<IteratedGamePairOutput> = results
        .into_iter()
        .filter(|((i, j), _)| !failed.contains(i) && !failed.contains(j))
        .map(|((i, j), history)| IteratedGamePairOutput {
            i: survivors.iter().position(|&x| x == i).unwrap(),
            j: survivors.iter().position(|&x| x == j).unwrap(),
            label_a: labels[i].clone(),
            label_b: labels[j].clone(),
            history,
        })
        .collect();

    (survivors, failed_sorted, games)
}

/// Build output for an iterated game tournament.
pub fn build_iterated_game_output(
    specs: &[StrategySpec],
    failed: Vec<usize>,
    games: Vec<IteratedGamePairOutput>,
    rounds: u32,
) -> IteratedGameTournamentOutput {
    let labels: Vec<String> = specs.iter().map(|s| s.label()).collect();
    let n = specs.len();
    IteratedGameTournamentOutput {
        strategies: labels,
        rounds,
        num_strategies: n,
        num_pairs: games.len(),
        games,
        failed,
    }
}

// ── Cross-product iterated game (left × right, history-returning) ───────────

/// Output for a cross-product iterated game batch.
#[derive(Serialize)]
pub struct IteratedGameCrossOutput {
    pub left_strategies: Vec<String>,
    pub right_strategies: Vec<String>,
    pub rounds: u32,
    pub num_left: usize,
    pub num_right: usize,
    pub num_pairs: usize,
    pub games: Vec<IteratedGamePairOutput>,
}

/// Run iterated games for the cross-product of two strategy sets (left × right).
/// Returns (left_survivors, right_survivors, game_outputs).
/// Non-halting strategies are excluded from both sets.
pub fn run_iterated_game_cross(
    left_specs: &[StrategySpec],
    right_specs: &[StrategySpec],
    rounds: u32,
    initial_history: &[[u8; 2]],
) -> (Vec<usize>, Vec<usize>, Vec<IteratedGamePairOutput>) {
    let nl = left_specs.len();
    let nr = right_specs.len();

    // Build cross-product pairs
    let pairs: Vec<(usize, usize)> = (0..nl)
        .flat_map(|i| (0..nr).map(move |j| (i, j)))
        .collect();

    eprintln!(
        "  CPU cross-product game: {} left x {} right = {} pairs, {} rounds (init_len={})",
        nl,
        nr,
        pairs.len(),
        rounds,
        initial_history.len()
    );

    use std::sync::Mutex;
    let left_failed: Mutex<std::collections::HashSet<usize>> =
        Mutex::new(std::collections::HashSet::new());
    let right_failed: Mutex<std::collections::HashSet<usize>> =
        Mutex::new(std::collections::HashSet::new());

    let results: Vec<((usize, usize), Vec<[u8; 2]>)> = pairs
        .par_iter()
        .map(|&(i, j)| {
            {
                let lf = left_failed.lock().unwrap();
                let rf = right_failed.lock().unwrap();
                if lf.contains(&i) || rf.contains(&j) {
                    return ((i, j), vec![]);
                }
            }
            let mut runner_a = StrategyRunner::new(&left_specs[i]);
            let mut runner_b = StrategyRunner::new(&right_specs[j]);
            let (history, failed_flag) =
                play_game_with_history(&mut runner_a, &mut runner_b, rounds, initial_history);
            if failed_flag != 0 {
                if failed_flag & 1 != 0 {
                    left_failed.lock().unwrap().insert(i);
                }
                if failed_flag & 2 != 0 {
                    right_failed.lock().unwrap().insert(j);
                }
            }
            ((i, j), history)
        })
        .collect();

    let lf = left_failed.into_inner().unwrap();
    let rf = right_failed.into_inner().unwrap();
    if !lf.is_empty() || !rf.is_empty() {
        eprintln!(
            "  Excluded {} left + {} right non-halting strategies",
            lf.len(),
            rf.len()
        );
    }

    let left_survivors: Vec<usize> = (0..nl).filter(|i| !lf.contains(i)).collect();
    let right_survivors: Vec<usize> = (0..nr).filter(|j| !rf.contains(j)).collect();

    let left_labels: Vec<String> = left_specs.iter().map(|s| s.label()).collect();
    let right_labels: Vec<String> = right_specs.iter().map(|s| s.label()).collect();

    let games: Vec<IteratedGamePairOutput> = results
        .into_iter()
        .filter(|((i, j), _)| !lf.contains(i) && !rf.contains(j))
        .map(|((i, j), history)| IteratedGamePairOutput {
            i: left_survivors.iter().position(|&x| x == i).unwrap(),
            j: right_survivors.iter().position(|&x| x == j).unwrap(),
            label_a: left_labels[i].clone(),
            label_b: right_labels[j].clone(),
            history,
        })
        .collect();

    (left_survivors, right_survivors, games)
}

/// Build output for a cross-product iterated game batch.
pub fn build_iterated_game_cross_output(
    left_specs: &[StrategySpec],
    right_specs: &[StrategySpec],
    games: Vec<IteratedGamePairOutput>,
    rounds: u32,
) -> IteratedGameCrossOutput {
    IteratedGameCrossOutput {
        left_strategies: left_specs.iter().map(|s| s.label()).collect(),
        right_strategies: right_specs.iter().map(|s| s.label()).collect(),
        rounds,
        num_left: left_specs.len(),
        num_right: right_specs.len(),
        num_pairs: games.len(),
        games,
    }
}

/// Write mixed tournament output as pretty JSON to stdout.
pub fn write_mixed_output(output: &MixedTournamentOutput) {
    let stdout = io::stdout();
    let mut out = BufWriter::new(stdout.lock());
    serde_json::to_writer_pretty(&mut out, output).ok();
    out.write_all(b"\n").ok();
    out.flush().ok();
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::decode_tm;

    #[test]
    fn parse_pd_game() {
        let payoff = parse_game("pd").unwrap();
        assert_eq!(payoff[0], [-1, -1]);  // CC
        assert_eq!(payoff[1], [-3, 0]);   // CD
        assert_eq!(payoff[2], [0, -3]);   // DC
        assert_eq!(payoff[3], [-2, -2]);  // DD
    }

    #[test]
    fn parse_chicken_game() {
        let payoff = parse_game("chicken").unwrap();
        assert_eq!(payoff[0], [0, 0]);
        assert_eq!(payoff[1], [-1, 1]);
        assert_eq!(payoff[2], [1, -1]);
        assert_eq!(payoff[3], [-10, -10]);
    }

    #[test]
    fn parse_custom_game() {
        let payoff = parse_game("1,2,3,4,5,6,7,8").unwrap();
        assert_eq!(payoff[0], [1, 2]);
        assert_eq!(payoff[1], [3, 4]);
        assert_eq!(payoff[2], [5, 6]);
        assert_eq!(payoff[3], [7, 8]);
    }

    #[test]
    fn parse_custom_game_bad_count() {
        assert!(parse_game("1,2,3").is_err());
    }

    #[test]
    fn parse_custom_game_bad_value() {
        assert!(parse_game("1,2,3,4,5,6,7,abc").is_err());
    }

    #[test]
    fn parse_ids_inline() {
        let ids = parse_ids(&Some("64,65,192".to_string()), &None).unwrap();
        assert_eq!(ids, vec![64, 65, 192]);
    }

    #[test]
    fn parse_ids_inline_spaces() {
        let ids = parse_ids(&Some(" 64 , 65 , 192 ".to_string()), &None).unwrap();
        assert_eq!(ids, vec![64, 65, 192]);
    }

    #[test]
    fn parse_ids_missing() {
        assert!(parse_ids(&None, &None).is_err());
    }

    #[test]
    fn all_pairs_count() {
        assert_eq!(all_pairs(3).len(), 9); // 3 * 3 (includes self-play)
        assert_eq!(all_pairs(5).len(), 25); // 5 * 5
    }

    #[test]
    fn all_pairs_includes_self() {
        let pairs = all_pairs(4);
        let self_pairs: Vec<_> = pairs.iter().filter(|(i, j)| i == j).collect();
        assert_eq!(self_pairs.len(), 4); // one self-play per machine
    }

    #[test]
    fn run_tm_on_tape_empty_tape() {
        // TM 64 on empty tape should halt (like input 0)
        let trans = decode_tm(64, 2, 2);
        let (halted, _out) = run_tm_on_tape(&trans, 2, &[], 500);
        assert!(halted);
    }

    #[test]
    fn run_tm_on_tape_matches_run_tm() {
        // For small inputs, run_tm_on_tape with the binary digits should
        // match run_tm with the integer input.
        let trans = decode_tm(64, 2, 2);
        for input in 0u64..16 {
            let (h1, out1, _) = crate::run_tm(&trans, 2, input, 500);
            let digits = crate::digits_in_base(input, 2);
            let (h2, out2) = run_tm_on_tape(&trans, 2, &digits, 500);
            assert_eq!(h1, h2, "halted mismatch for input {}", input);
            if h1 {
                assert_eq!(out1, out2, "output mismatch for input {}", input);
            }
        }
    }

    #[test]
    fn run_tm_on_tape_non_halting() {
        // TM 0 never halts
        let trans = decode_tm(0, 2, 2);
        let (halted, _) = run_tm_on_tape(&trans, 2, &[0, 1], 100);
        assert!(!halted);
    }

    #[test]
    fn play_game_self_cooperator() {
        // TM 64 always outputs 0 (cooperate). In PD, CC = (-1,-1) each round.
        // Self vs self for 10 rounds → score = -10 each.
        let trans = decode_tm(64, 2, 2);
        let payoff = parse_game_dyn("pd").unwrap();
        let ((sa, sb), _failed) = play_game_cpu(&trans, &trans, 2, 500, 10, &payoff);
        assert_eq!(sa, -10);
        assert_eq!(sb, -10);
    }

    #[test]
    fn play_game_cooperator_vs_defector_short() {
        // TM 64 cooperates. Find a defector TM (always outputs 1).
        // We'll build one manually: just need a TM where output is always 1.
        // For now, test that round 0 both cooperate.
        let trans = decode_tm(64, 2, 2);
        let payoff = parse_game_dyn("pd").unwrap();
        let ((sa, _sb), _failed) = play_game_cpu(&trans, &trans, 2, 500, 1, &payoff);
        // Round 0: both cooperate → CC = (-1, -1)
        assert_eq!(sa, -1);
    }

    #[test]
    fn tournament_small_cpu() {
        // Run a tiny tournament with 3 known-halting TMs
        let ids = vec![64, 65, 192];
        let payoff = parse_game_dyn("pd").unwrap();
        let (survivors, a_scores, b_scores) = run_tournament_cpu(&ids, 2, 2, 500, 10, &payoff);
        let m = survivors.len();
        assert_eq!(a_scores.len(), m);
        assert_eq!(b_scores.len(), m);
        assert_eq!(a_scores[0].len(), m);
        assert_eq!(b_scores[0].len(), m);
        // With self-play (Tuples convention), diagonal may be non-zero
        // Just verify the matrix dimensions are correct
        for i in 0..m {
            assert_eq!(a_scores[i].len(), m);
            assert_eq!(b_scores[i].len(), m);
        }
    }

    #[test]
    fn tournament_output_ranking_sorted() {
        let ids = vec![64, 65];
        let a_scores = vec![vec![0, -10], vec![-5, 0]];
        let b_scores = vec![vec![0, -5], vec![-10, 0]];
        let output = build_output(&ids, a_scores, b_scores, 10, "pd", 2, 2);
        // Ranking should be sorted by mean descending
        assert!(output.ranking[0].mean >= output.ranking[1].mean);
    }

    // ── Mixed tournament tests ───────────────────────────────────────────

    #[test]
    fn parse_strategies_inline_basic() {
        let ndjson = r#"{"type":"tm","id":64,"s":2,"k":2}
{"type":"fsm","id":0,"s":1,"k":2}
{"type":"ca","rule":110,"k":2,"r":1.0,"t":10}"#;
        let specs = parse_strategies_inline(ndjson).unwrap();
        assert_eq!(specs.len(), 3);
    }

    #[test]
    fn parse_strategies_inline_with_comments() {
        let ndjson = "# comment\n{\"type\":\"tm\",\"id\":64,\"s\":2,\"k\":2}\n\n";
        let specs = parse_strategies_inline(ndjson).unwrap();
        assert_eq!(specs.len(), 1);
    }

    #[test]
    fn parse_strategies_inline_bad_json() {
        let ndjson = "not json";
        assert!(parse_strategies_inline(ndjson).is_err());
    }

    #[test]
    fn mixed_tournament_tm_only() {
        // A mixed tournament with only TMs should produce the same results
        // as the TM-only tournament.
        let specs = vec![
            StrategySpec::Tm { id: "64".to_string(), s: 2, k: 2, max_steps: 500, num_actions: 2 },
            StrategySpec::Tm { id: "65".to_string(), s: 2, k: 2, max_steps: 500, num_actions: 2 },
        ];
        let payoff = parse_game_dyn("pd").unwrap();
        let (survivors, a_scores, b_scores) = run_mixed_tournament_cpu(&specs, 10, &payoff);
        assert_eq!(survivors.len(), 2); // both TMs halt
        assert_eq!(a_scores.len(), 2);
        assert_eq!(b_scores.len(), 2);
        // With self-play, diagonal is non-zero (self vs self game)
        assert_eq!(a_scores[0].len(), 2);
        assert_eq!(a_scores[1].len(), 2);
        assert_eq!(b_scores[0].len(), 2);
        assert_eq!(b_scores[1].len(), 2);
    }

    #[test]
    fn mixed_tournament_fsm_cooperator_vs_defector() {
        let specs = vec![
            StrategySpec::Fsm { id: "1".to_string(), s: 1, k: 2, num_actions: 2 },
            StrategySpec::Fsm { id: "0".to_string(), s: 1, k: 2, num_actions: 2 },
        ];
        let payoff = parse_game_dyn("pd").unwrap();
        let (survivors, a_scores, b_scores) = run_mixed_tournament_cpu(&specs, 10, &payoff);
        assert_eq!(survivors.len(), 2); // FSMs always halt
        // FSM id=1 cooperates (action 1%2=1 -> defect? or cooperate?)
        // a_scores[0][1] = player 0's score as A vs player 1 as B
        assert_eq!(a_scores[0][1], -30);
        assert_eq!(a_scores[1][0], 0);
    }

    #[test]
    fn mixed_tournament_three_types() {
        let specs = vec![
            StrategySpec::Tm { id: "64".to_string(), s: 2, k: 2, max_steps: 500, num_actions: 2 },
            StrategySpec::Fsm { id: "0".to_string(), s: 1, k: 2, num_actions: 2 },
            StrategySpec::Ca { rule: "110".to_string(), k: 2, r: 1.0, t: 2, num_actions: 2 },
        ];
        let payoff = parse_game_dyn("pd").unwrap();
        let (survivors, a_scores, b_scores) = run_mixed_tournament_cpu(&specs, 10, &payoff);
        assert_eq!(survivors.len(), 3); // all halt
        assert_eq!(a_scores.len(), 3);
        assert_eq!(b_scores.len(), 3);
        for i in 0..3 {
            assert_eq!(a_scores[i].len(), 3);
            assert_eq!(b_scores[i].len(), 3);
        }
    }

    #[test]
    fn mixed_output_ranking_sorted() {
        let specs = vec![
            StrategySpec::Fsm { id: "0".to_string(), s: 1, k: 2, num_actions: 2 },
            StrategySpec::Fsm { id: "1".to_string(), s: 1, k: 2, num_actions: 2 },
        ];
        let a_scores = vec![vec![0, -30], vec![0, 0]];
        let b_scores = vec![vec![0, 0], vec![-30, 0]];
        let output = build_mixed_output(&specs, a_scores, b_scores, 10, "pd");
        assert!(output.ranking[0].mean >= output.ranking[1].mean);
    }
}
