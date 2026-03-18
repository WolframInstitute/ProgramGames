use clap::{Parser, ValueEnum};
use rayon::prelude::*;
use serde::Serialize;
use std::io::{self, BufWriter, Write};
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};

mod gpu;
mod strategy;
mod tournament;

// ── CLI ─────────────────────────────────────────────────────────────────────

#[derive(Parser, Debug)]
#[command(name = "tm-search", version, about = "Find always-halting Turing machines for iterated games")]
struct Cli {
    #[command(subcommand)]
    command: Cmd,
}

#[derive(clap::Subcommand, Debug)]
enum Cmd {
    /// Enumerate / sample TMs and output those that halt on every game input.
    Search {
        /// Number of internal states.
        #[arg(short, long)]
        states: u16,

        /// Number of tape symbols.
        #[arg(short = 'k', long)]
        symbols: u8,

        /// Max computation steps before declaring non-halting.
        #[arg(short, long, default_value_t = 500)]
        max_steps: u32,

        /// Test halting on all game histories up to this many rounds.
        #[arg(short, long, default_value_t = 4)]
        depth: u32,

        /// Random-sample this many candidates (omit for full enumeration).
        #[arg(long)]
        sample: Option<u64>,

        /// Start of index range (inclusive, default 0).
        #[arg(long, default_value_t = 0)]
        start: u64,

        /// End of index range (inclusive, default = max index).
        #[arg(long)]
        end: Option<u64>,

        /// Output format.
        #[arg(long, value_enum, default_value_t = Format::Json)]
        format: Format,

        /// Also include the output (Mod 2) for each test input in the JSON.
        #[arg(long)]
        with_outputs: bool,

        /// Use Metal GPU acceleration (macOS). Falls back to CPU if unavailable.
        #[arg(long)]
        gpu: bool,
    },

    /// Print the max TM index for given states/symbols.
    MaxIndex {
        #[arg(short, long)]
        states: u16,
        #[arg(short = 'k', long)]
        symbols: u8,
    },

    /// Print space size info for given states/symbols.
    Info {
        #[arg(short, long)]
        states: u16,
        #[arg(short = 'k', long)]
        symbols: u8,
    },

    /// Classify TMs by behavior: group machines that produce identical outputs.
    Classify {
        #[arg(short, long)]
        states: u16,
        #[arg(short = 'k', long)]
        symbols: u8,
        #[arg(short, long, default_value_t = 500)]
        max_steps: u32,
        /// Signature depth: test inputs up to this many rounds of history.
        #[arg(short, long, default_value_t = 4)]
        depth: u32,
        #[arg(long)]
        gpu: bool,
        /// Only classify these TM IDs (otherwise enumerates all halting).
        #[arg(long)]
        ids: Option<String>,
        #[arg(long)]
        ids_file: Option<PathBuf>,
    },

    /// Run an iterated-game tournament among a set of TMs (or mixed TM/FSM/CA strategies).
    Tournament {
        /// Number of internal states (required for TM-only mode with --ids).
        #[arg(short, long)]
        states: Option<u16>,
        /// Number of tape symbols (required for TM-only mode with --ids).
        #[arg(short = 'k', long)]
        symbols: Option<u8>,
        #[arg(short, long, default_value_t = 500)]
        max_steps: u32,
        #[arg(short, long, default_value_t = 100)]
        rounds: u32,
        /// Game: pd, chicken, or custom payoff "CC_a,CC_b,CD_a,CD_b,DC_a,DC_b,DD_a,DD_b"
        #[arg(long, default_value = "pd", allow_hyphen_values = true)]
        game: String,
        /// File with one TM ID per line (or "-" for stdin). Requires --states and --symbols.
        #[arg(long)]
        ids_file: Option<PathBuf>,
        /// Inline comma-separated TM IDs. Requires --states and --symbols.
        #[arg(long)]
        ids: Option<String>,
        /// NDJSON file with strategy specs (or "-" for stdin). Supports TM, FSM, CA.
        #[arg(long)]
        strategies_file: Option<PathBuf>,
        /// Inline NDJSON strategy specs (one per line).
        #[arg(long)]
        strategies: Option<String>,
        #[arg(long)]
        gpu: bool,
        #[arg(long, value_enum, default_value_t = Format::Json)]
        format: Format,
    },
}

#[derive(Copy, Clone, Debug, ValueEnum)]
enum Format {
    /// One JSON object per halting TM (NDJSON).
    Json,
    /// Only the TM index, one per line.
    Ids,
}

// ── TM core (matches Wolfram + nit-games encoding) ──────────────────────────

#[derive(Copy, Clone, Debug)]
pub(crate) struct TmTransition {
    pub write: u8,
    pub move_right: bool, // true = Right, false = Left
    pub next: u16,        // 1-indexed state
}

/// Decode a TM rule code to its transition table.
/// Encoding matches `ResourceFunction["TuringMachineFromNumber"]` and
/// `decode_tm_rule_code_wolfram` in nit-games.
fn decode_tm(rule_code: u64, states: u16, symbols: u8) -> Vec<TmTransition> {
    let s = states as usize;
    let k = symbols as usize;
    let total = s * k;
    let base = (k * s * 2) as u64;

    let mut transitions = vec![
        TmTransition {
            write: 0,
            move_right: false,
            next: 1,
        };
        total
    ];

    if base == 0 {
        return transitions;
    }

    let mut code = rule_code;
    for state in (1..=s).rev() {
        for read in 0..k {
            let digit = code % base;
            code /= base;
            let move_right = (digit % 2) == 1;
            let write = ((digit / 2) % k as u64) as u8;
            let next = (digit / (2 * k as u64)) as u16 + 1;
            let idx = (state - 1) * k + read;
            transitions[idx] = TmTransition {
                write,
                move_right,
                next,
            };
        }
    }
    transitions
}

/// Max TM index for (s, k): (2*s*k)^(s*k) - 1.
fn tm_max_index(states: u16, symbols: u8) -> Option<u128> {
    let s = states as u128;
    let k = symbols as u128;
    let base = 2u128.checked_mul(s)?.checked_mul(k)?;
    let exp = (states as u32).checked_mul(symbols as u32)?;
    checked_pow(base, exp)?.checked_sub(1)
}

fn checked_pow(base: u128, exp: u32) -> Option<u128> {
    let mut result = 1u128;
    for _ in 0..exp {
        result = result.checked_mul(base)?;
    }
    Some(result)
}

/// Convert integer to MSD-first base-k digits.
fn digits_in_base(mut value: u64, base: u8) -> Vec<u8> {
    let base = base.max(2) as u64;
    if value == 0 {
        return vec![0];
    }
    let mut digits = Vec::new();
    while value > 0 {
        digits.push((value % base) as u8);
        value /= base;
    }
    digits.reverse();
    digits
}

/// Run a one-sided TM on integer input. Returns (halted, output_mod2, steps).
/// Matches the nit-games `run_one_sided_tm` exactly:
///   - tape = digits of input in base `symbols`
///   - head starts at rightmost cell
///   - halts when moving Right past tape end
///   - blank = 0
fn run_tm(
    transitions: &[TmTransition],
    symbols: u8,
    input: u64,
    max_steps: u32,
) -> (bool, u8, u32) {
    let k = symbols.max(2);
    let digits = digits_in_base(input, k);
    let mut tape = digits;
    let mut head: usize = tape.len().saturating_sub(1);
    let mut state: u16 = 1; // start state

    for step in 0..max_steps {
        let read = tape.get(head).copied().unwrap_or(0);
        let idx = (state.saturating_sub(1) as usize) * (k as usize) + (read as usize);
        let Some(&trans) = transitions.get(idx) else {
            return (false, 0, step + 1);
        };

        if let Some(cell) = tape.get_mut(head) {
            *cell = trans.write;
        }

        // Halt: move Right past end of tape
        if trans.move_right && head + 1 == tape.len() {
            let out_symbol = tape.last().copied().unwrap_or(0);
            return (true, out_symbol % 2, step + 1);
        }

        if trans.move_right {
            if head + 1 < tape.len() {
                head += 1;
            }
        } else {
            // Left
            if head == 0 {
                tape.insert(0, 0); // blank
                // head stays at 0
            } else {
                head -= 1;
            }
        }

        state = trans.next;
        if state == 0 {
            return (false, 0, step + 1);
        }
    }

    (false, 0, max_steps)
}

/// Test if a TM halts on ALL game-relevant inputs up to `depth` rounds.
/// Returns `Some(outputs)` if always halts, `None` if any input fails.
///
/// Uses two key optimizations:
///
/// 1. **Adaptive depth**: A one-sided TM that halts in t steps reads at
///    most t+1 cells from the right end of the tape. If worst-case steps
///    at depth d satisfy w+1 ≤ 2*d, all longer inputs share the same
///    suffix patterns → skip remaining depths. Most TMs stop at depth 4-5.
///
/// 2. **Pre-padded tape with dirty tracking**: For TMs that DO need deep
///    testing (depth ≥ 5), uses a pre-allocated buffer with left padding
///    and only clears the region dirtied by the previous run. This avoids
///    the O(n²) cost of Vec::insert(0, _) and the O(max_steps) cost of
///    full-buffer clearing.
fn test_halting(
    transitions: &[TmTransition],
    symbols: u8,
    max_steps: u32,
    depth: u32,
) -> Option<Vec<(u64, u8)>> {
    let k = symbols as u64;
    let mut outputs = Vec::new();
    let mut worst_steps: u32 = 0;

    for r in 1..=depth {
        // Adaptive skip: if TM reads at most worst_steps+1 cells, and
        // previous depth already covered all patterns of that width, done.
        if r > 1 && worst_steps + 1 <= (2 * (r - 1)) as u32 {
            return Some(outputs);
        }

        let max_input = k.checked_pow(2 * r).unwrap_or(u64::MAX);
        for input in 0..max_input {
            let (halted, out_mod2, steps) = run_tm(transitions, symbols, input, max_steps);
            if !halted {
                return None;
            }
            worst_steps = worst_steps.max(steps);
            outputs.push((input, out_mod2));
        }
    }
    Some(outputs)
}

// ── Output structs ──────────────────────────────────────────────────────────

#[derive(Serialize)]
struct HaltingTm {
    id: u64,
    s: u16,
    k: u8,
    #[serde(skip_serializing_if = "Option::is_none")]
    max_steps_used: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    outputs: Option<Vec<(u64, u8)>>,
}

#[derive(Serialize)]
struct SearchSummary {
    states: u16,
    symbols: u8,
    max_steps: u32,
    depth: u32,
    space_size: String,
    searched: u64,
    halting_count: u64,
    halting_fraction: f64,
}

// ── GPU-accelerated search ───────────────────────────────────────────────

/// Run the halting search using Metal GPU + CPU adaptive depth.
/// Phase 1 (CPU): depth 1-4, adaptive skip.
/// Phase 2 (GPU): for remaining TMs, dispatch per-depth batches.
/// Falls back to CPU-only if GPU init fails.
#[cfg(all(target_os = "macos", feature = "metal"))]
fn search_gpu(
    candidates: &[u64],
    states: u16,
    symbols: u8,
    max_steps: u32,
    depth: u32,
) -> Vec<u64> {
    use crate::gpu::MetalSearcher;

    let gpu = match MetalSearcher::new() {
        Ok(g) => g,
        Err(e) => {
            eprintln!("  Metal init failed ({}), falling back to CPU", e);
            return search_cpu(candidates, states, symbols, max_steps, depth);
        }
    };

    let k = symbols as u64;
    let gpu_max_steps = max_steps.min(10000); // GPU tape limit (MAX_TAPE=11000)

    // Phase 1 (CPU): test all TMs at depth 1-4, record worst_steps
    eprintln!("  GPU phase 1: CPU depth 1-4 filter...");
    struct TmState {
        id: u64,
        transitions: Vec<TmTransition>,
        worst_steps: u32,
        alive: bool,
    }

    let mut tms: Vec<TmState> = candidates
        .par_iter()
        .map(|&id| {
            let transitions = decode_tm(id, states, symbols);
            let mut worst = 0u32;
            let mut alive = true;
            for r in 1..=depth.min(4) {
                if r > 1 && worst + 1 <= (2 * (r - 1)) as u32 {
                    // suffix-complete
                    break;
                }
                let max_input = k.checked_pow(2 * r).unwrap_or(u64::MAX);
                for input in 0..max_input {
                    let (halted, _, steps) = run_tm(&transitions, symbols, input, max_steps);
                    if !halted {
                        alive = false;
                        break;
                    }
                    worst = worst.max(steps);
                }
                if !alive { break; }
            }
            TmState { id, transitions, worst_steps: worst, alive }
        })
        .collect();

    // Remove non-halting and suffix-complete
    let phase1_halting: Vec<u64> = tms.iter()
        .filter(|t| t.alive && t.worst_steps + 1 <= (2 * depth.min(4)) as u32)
        .map(|t| t.id)
        .collect();

    let mut need_gpu: Vec<&mut TmState> = tms.iter_mut()
        .filter(|t| t.alive && t.worst_steps + 1 > (2 * depth.min(4)) as u32)
        .collect();

    eprintln!("  GPU phase 1 done: {} halting (suffix-complete), {} need deeper testing",
              phase1_halting.len(), need_gpu.len());

    if need_gpu.is_empty() || depth <= 4 {
        return tms.iter().filter(|t| t.alive).map(|t| t.id).collect();
    }

    // Phase 2 (GPU): test remaining TMs depth-by-depth
    let batch_size = 65536usize;
    for r in (depth.min(4) + 1)..=depth {
        // Skip TMs that are suffix-complete
        need_gpu.retain(|t| t.alive && t.worst_steps + 1 > (2 * (r - 1)) as u32);
        if need_gpu.is_empty() { break; }

        let input_width = (2 * r) as u32;
        let num_inputs = k.checked_pow(2 * r).unwrap_or(u64::MAX);
        if num_inputs > u32::MAX as u64 {
            eprintln!("  Depth {} has too many inputs for GPU, falling back to CPU", r);
            // CPU fallback for this depth
            for tm in need_gpu.iter_mut() {
                for input in 0..num_inputs {
                    let (halted, _, steps) = run_tm(&tm.transitions, symbols, input, max_steps);
                    if !halted { tm.alive = false; break; }
                    tm.worst_steps = tm.worst_steps.max(steps);
                }
            }
            continue;
        }

        let inputs: Vec<u64> = (0..num_inputs).collect();

        eprintln!("  GPU depth {}: {} TMs × {} inputs = {} pairs",
                  r, need_gpu.len(), num_inputs, need_gpu.len() as u64 * num_inputs);

        // Process in GPU batches
        for batch_start in (0..need_gpu.len()).step_by(batch_size) {
            let batch_end = (batch_start + batch_size).min(need_gpu.len());
            let batch_trans: Vec<Vec<TmTransition>> = need_gpu[batch_start..batch_end]
                .iter()
                .map(|t| t.transitions.clone())
                .collect();

            // Per-TM step limit: worst_steps * 4 + margin, capped at gpu_max_steps.
            // Deeper inputs may need more steps, so we add headroom.
            let batch_limits: Vec<u32> = need_gpu[batch_start..batch_end]
                .iter()
                .map(|t| (t.worst_steps.saturating_mul(4).saturating_add(64)).min(gpu_max_steps))
                .collect();

            match gpu.test_batch(&batch_trans, &inputs, states, symbols,
                                 gpu_max_steps, input_width, &batch_limits) {
                Ok(results) => {
                    for (i, &all_halted) in results.iter().enumerate() {
                        if !all_halted {
                            let tm = &mut need_gpu[batch_start + i];
                            // If the per-TM limit was < max_steps, CPU-verify
                            // with full limit before declaring non-halting.
                            if batch_limits[i] < max_steps {
                                let mut still_alive = true;
                                for input in 0..num_inputs {
                                    let (h, _, s) = run_tm(&tm.transitions, symbols, input, max_steps);
                                    if !h { still_alive = false; break; }
                                    tm.worst_steps = tm.worst_steps.max(s);
                                }
                                if !still_alive { tm.alive = false; }
                            } else {
                                tm.alive = false;
                            }
                        }
                    }
                }
                Err(e) => {
                    eprintln!("  GPU batch failed ({}), CPU fallback for batch", e);
                    for tm in &mut need_gpu[batch_start..batch_end] {
                        for input in 0..num_inputs {
                            let (halted, _, _) = run_tm(&tm.transitions, symbols, input, max_steps);
                            if !halted { tm.alive = false; break; }
                        }
                    }
                }
            }
        }

        // CPU re-verification for low-limit failures is handled inline above.
    }

    tms.iter().filter(|t| t.alive).map(|t| t.id).collect()
}

/// CPU-only search (existing logic).
fn search_cpu(
    candidates: &[u64],
    states: u16,
    symbols: u8,
    max_steps: u32,
    depth: u32,
) -> Vec<u64> {
    candidates
        .par_iter()
        .with_min_len(1)
        .filter_map(|&id| {
            let transitions = decode_tm(id, states, symbols);
            if test_halting(&transitions, symbols, max_steps, depth).is_some() {
                Some(id)
            } else {
                None
            }
        })
        .collect()
}

// ── Main ────────────────────────────────────────────────────────────────────

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Cmd::MaxIndex { states, symbols } => {
            match tm_max_index(states, symbols) {
                Some(max) => println!("{}", max),
                None => {
                    eprintln!("Overflow: space too large for u128");
                    std::process::exit(1);
                }
            }
        }

        Cmd::Info { states, symbols } => {
            let max = tm_max_index(states, symbols);
            let count = max.map(|m| m + 1);
            println!("TM({}, {})", states, symbols);
            println!("  base   = 2 * {} * {} = {}", states, symbols, 2 * states as u32 * symbols as u32);
            println!("  exp    = {} * {} = {}", states, symbols, states as u32 * symbols as u32);
            match count {
                Some(c) => println!("  count  = {}", c),
                None => println!("  count  = OVERFLOW (> 2^128)"),
            }
            match max {
                Some(m) => println!("  max_id = {}", m),
                None => println!("  max_id = OVERFLOW"),
            }
        }

        Cmd::Search {
            states,
            symbols,
            max_steps,
            depth,
            sample,
            start,
            end,
            format,
            with_outputs,
            gpu: use_gpu,
        } => {
            let max_id = match tm_max_index(states, symbols) {
                Some(m) if m <= u64::MAX as u128 => m as u64,
                Some(_) => {
                    eprintln!("Space too large for exhaustive search without --sample");
                    if sample.is_none() {
                        std::process::exit(1);
                    }
                    u64::MAX
                }
                None => {
                    eprintln!("Overflow computing space size");
                    std::process::exit(1);
                }
            };

            let range_end = end.unwrap_or(max_id).min(max_id);
            let total_in_range = range_end - start + 1;

            // Build candidate list
            let candidates: Vec<u64> = match sample {
                Some(n) => {
                    // Random sample with full 64-bit range (deterministic PCG-like)
                    use std::collections::HashSet;
                    let mut seen = HashSet::new();
                    let mut rng_state = 0x5DEECE66Du64.wrapping_mul(42).wrapping_add(0xBu64);
                    let target = n.min(total_in_range);
                    while (seen.len() as u64) < target {
                        // Two LCG iterations to produce a full 64-bit value
                        rng_state = rng_state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                        let hi = rng_state >> 32;
                        rng_state = rng_state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                        let lo = rng_state >> 32;
                        let full = (hi << 32) | lo;
                        let idx = start + full % total_in_range;
                        seen.insert(idx);
                    }
                    let mut v: Vec<u64> = seen.into_iter().collect();
                    v.sort();
                    v
                }
                None => (start..=range_end).collect(),
            };

            let searched = candidates.len() as u64;
            eprintln!(
                "tm-search: TM({},{}) | range [{}, {}] | candidates {} | max_steps {} | depth {}",
                states, symbols, start, range_end, searched, max_steps, depth
            );

            let found = AtomicU64::new(0);

            // Choose GPU or CPU search path
            let halting_ids: Vec<u64>;

            #[cfg(all(target_os = "macos", feature = "metal"))]
            {
                if use_gpu {
                    halting_ids = search_gpu(&candidates, states, symbols, max_steps, depth);
                } else {
                    halting_ids = search_cpu(&candidates, states, symbols, max_steps, depth);
                }
            }
            #[cfg(not(all(target_os = "macos", feature = "metal")))]
            {
                if use_gpu {
                    eprintln!("  Metal not compiled in, using CPU");
                }
                halting_ids = search_cpu(&candidates, states, symbols, max_steps, depth);
            }

            let halting_count = halting_ids.len() as u64;
            found.store(halting_count, Ordering::Relaxed);

            // Format output
            let results: Vec<Vec<u8>> = halting_ids
                .par_iter()
                .map(|&id| {
                    let mut buf = Vec::new();
                    match format {
                        Format::Json => {
                            let max_steps_used = {
                                let transitions = decode_tm(id, states, symbols);
                                let mut worst = 0u32;
                                for r in 1..=depth.min(4) {
                                    let max_input = (symbols as u64).checked_pow(2 * r).unwrap_or(u64::MAX);
                                    for input in 0..max_input {
                                        let (_, _, steps) = run_tm(&transitions, symbols, input, max_steps);
                                        worst = worst.max(steps);
                                    }
                                }
                                worst
                            };
                            let record = HaltingTm {
                                id,
                                s: states,
                                k: symbols,
                                max_steps_used: Some(max_steps_used),
                                outputs: if with_outputs {
                                    let transitions = decode_tm(id, states, symbols);
                                    test_halting(&transitions, symbols, max_steps, depth)
                                } else {
                                    None
                                },
                            };
                            serde_json::to_writer(&mut buf, &record).ok();
                            buf.push(b'\n');
                        }
                        Format::Ids => {
                            use std::io::Write;
                            write!(&mut buf, "{}\n", id).ok();
                        }
                    }
                    buf
                })
                .collect();
            eprintln!("\r  done: {}/{} halting ({:.2}%)                    ",
                      halting_count, searched,
                      halting_count as f64 / searched as f64 * 100.0);

            // Write all results to stdout
            let stdout = io::stdout();
            let mut out = BufWriter::new(stdout.lock());
            for chunk in &results {
                if !chunk.is_empty() {
                    out.write_all(chunk).ok();
                }
            }
            out.flush().ok();

            // Print summary to stderr
            let summary = SearchSummary {
                states,
                symbols,
                max_steps,
                depth,
                space_size: tm_max_index(states, symbols)
                    .map(|m| (m + 1).to_string())
                    .unwrap_or_else(|| "OVERFLOW".to_string()),
                searched,
                halting_count,
                halting_fraction: halting_count as f64 / searched as f64,
            };
            eprintln!("\n{}", serde_json::to_string_pretty(&summary).unwrap());
        }

        Cmd::Classify {
            states,
            symbols,
            max_steps,
            depth,
            gpu: use_gpu,
            ids,
            ids_file,
        } => {
            // Get TM IDs: from args, or find all halting
            let tm_ids = if ids.is_some() || ids_file.is_some() {
                tournament::parse_ids(&ids, &ids_file).unwrap_or_else(|e| {
                    eprintln!("Error: {}", e);
                    std::process::exit(1);
                })
            } else {
                eprintln!("Finding halting TM({},{})...", states, symbols);
                #[cfg(all(target_os = "macos", feature = "metal"))]
                {
                    if use_gpu {
                        search_gpu(
                            &(0..=tm_max_index(states, symbols).unwrap_or(0) as u64).collect::<Vec<_>>(),
                            states, symbols, max_steps, depth,
                        )
                    } else {
                        search_cpu(
                            &(0..=tm_max_index(states, symbols).unwrap_or(0) as u64).collect::<Vec<_>>(),
                            states, symbols, max_steps, depth,
                        )
                    }
                }
                #[cfg(not(all(target_os = "macos", feature = "metal")))]
                {
                    let max_id = tm_max_index(states, symbols).unwrap_or(0) as u64;
                    search_cpu(&(0..=max_id).collect::<Vec<_>>(), states, symbols, max_steps, depth)
                }
            };

            eprintln!("Classifying {} halting TMs by behavior (depth {})...", tm_ids.len(), depth);

            // Compute behavior signature for each TM.
            // We already know these TMs halt. Just compute output mod 2
            // on depth-4 inputs (340 for k=2). This is a lean loop — no
            // adaptive depth, no Vec allocation per input, no halting check.
            let classify_depth = depth.min(4);
            let k = symbols as u64;

            let signatures: Vec<(u64, Vec<u8>)> = tm_ids
                .par_iter()
                .map(|&id| {
                    let trans = decode_tm(id, states, symbols);
                    let mut sig = Vec::new();
                    for r in 1..=classify_depth {
                        let max_input = k.pow(2 * r);
                        for input in 0..max_input {
                            let (halted, out, _) = run_tm(&trans, symbols, input, max_steps);
                            sig.push(if halted { out } else { 1 });
                        }
                    }
                    (id, sig)
                })
                .collect();

            // Group by signature
            use std::collections::HashMap;
            let mut groups: HashMap<Vec<u8>, Vec<u64>> = HashMap::new();
            for (id, sig) in &signatures {
                groups.entry(sig.clone()).or_default().push(*id);
            }

            // Sort groups by size descending
            let mut sorted_groups: Vec<(Vec<u8>, Vec<u64>)> = groups.into_iter().collect();
            sorted_groups.sort_by(|a, b| b.1.len().cmp(&a.1.len()));

            eprintln!("  {} halting TMs -> {} unique behaviors ({}x reduction)",
                      signatures.len(), sorted_groups.len(),
                      if sorted_groups.is_empty() { 0.0 } else {
                          signatures.len() as f64 / sorted_groups.len() as f64
                      });

            // Output JSON
            #[derive(Serialize)]
            struct ClassifyOutput {
                states: u16,
                symbols: u8,
                depth: u32,
                max_steps: u32,
                total_halting: usize,
                unique_behaviors: usize,
                reduction_factor: f64,
                representatives: Vec<u64>,
                groups: Vec<GroupEntry>,
            }
            #[derive(Serialize)]
            struct GroupEntry {
                representative: u64,
                count: usize,
                members: Vec<u64>,
                signature_preview: String,
            }

            let output = ClassifyOutput {
                states,
                symbols,
                depth,
                max_steps,
                total_halting: signatures.len(),
                unique_behaviors: sorted_groups.len(),
                reduction_factor: if sorted_groups.is_empty() { 0.0 } else {
                    signatures.len() as f64 / sorted_groups.len() as f64
                },
                representatives: sorted_groups.iter().map(|(_, ids)| ids[0]).collect(),
                groups: sorted_groups.iter().map(|(sig, ids)| {
                    let preview: String = sig.iter().take(32).map(|b| char::from(b'0' + b)).collect();
                    let preview = if sig.len() > 32 {
                        format!("{}...", preview)
                    } else {
                        preview
                    };
                    GroupEntry {
                        representative: ids[0],
                        count: ids.len(),
                        members: ids.clone(),
                        signature_preview: preview,
                    }
                }).collect(),
            };

            let stdout = io::stdout();
            let mut out = BufWriter::new(stdout.lock());
            serde_json::to_writer_pretty(&mut out, &output).ok();
            out.write_all(b"\n").ok();
            out.flush().ok();
        }

        Cmd::Tournament {
            states,
            symbols,
            max_steps,
            rounds,
            game,
            ids_file,
            ids,
            strategies_file,
            strategies,
            gpu: use_gpu,
            format: _format,
        } => {
            // Parse game payoffs
            let dyn_payoff = match tournament::parse_game_dyn(&game) {
                Ok(p) => p,
                Err(e) => {
                    eprintln!("Error: {}", e);
                    std::process::exit(1);
                }
            };

            // Determine mode: mixed strategies or TM-only
            let has_strategies = strategies_file.is_some() || strategies.is_some();
            let has_ids = ids_file.is_some() || ids.is_some();

            if has_strategies {
                // Mixed-strategy mode
                let specs = if let Some(ref ndjson) = strategies {
                    match tournament::parse_strategies_inline(ndjson) {
                        Ok(v) => v,
                        Err(e) => {
                            eprintln!("Error: {}", e);
                            std::process::exit(1);
                        }
                    }
                } else if let Some(ref path) = strategies_file {
                    match tournament::parse_strategies_file(path) {
                        Ok(v) => v,
                        Err(e) => {
                            eprintln!("Error: {}", e);
                            std::process::exit(1);
                        }
                    }
                } else {
                    unreachable!()
                };

                if specs.is_empty() {
                    eprintln!("Error: no strategies provided");
                    std::process::exit(1);
                }

                eprintln!(
                    "tm-tournament (mixed): {} strategies | {} rounds | game={}",
                    specs.len(), rounds, game
                );
                for (i, spec) in specs.iter().enumerate() {
                    eprintln!("  [{}] {}", i, spec.label());
                }

                let (survivors, scores) = tournament::run_mixed_tournament_cpu(&specs, rounds, &dyn_payoff);
                let surviving_specs: Vec<_> = survivors.iter().map(|&i| specs[i].clone()).collect();
                let output = tournament::build_mixed_output(&surviving_specs, scores, rounds, &game);

                eprintln!("  done: {} strategies survived, {} pairs scored",
                          surviving_specs.len(), output.num_pairs);
                tournament::write_mixed_output(&output);
            } else if has_ids {
                // TM-only mode (legacy)
                let states = match states {
                    Some(s) => s,
                    None => {
                        eprintln!("Error: --states is required for TM-only mode (--ids)");
                        std::process::exit(1);
                    }
                };
                let symbols = match symbols {
                    Some(k) => k,
                    None => {
                        eprintln!("Error: --symbols is required for TM-only mode (--ids)");
                        std::process::exit(1);
                    }
                };

                let tm_ids = match tournament::parse_ids(&ids, &ids_file) {
                    Ok(v) => v,
                    Err(e) => {
                        eprintln!("Error: {}", e);
                        std::process::exit(1);
                    }
                };
                if tm_ids.is_empty() {
                    eprintln!("Error: no TM IDs provided");
                    std::process::exit(1);
                }

                eprintln!(
                    "tm-tournament: TM({},{}) | {} machines | {} rounds | game={} | max_steps={}",
                    states, symbols, tm_ids.len(), rounds, game, max_steps
                );

                // Dispatch to GPU or CPU
                let scores: Vec<Vec<i64>>;

                #[cfg(all(target_os = "macos", feature = "metal"))]
                {
                    if use_gpu {
                        scores = match gpu::run_tournament_gpu(
                            &tm_ids, states, symbols, max_steps, rounds,
                            dyn_payoff.num_actions as u32, &dyn_payoff.entries,
                        ) {
                            Ok(s) => s,
                            Err(e) => {
                                eprintln!("  GPU tournament failed ({}), falling back to CPU", e);
                                tournament::run_tournament_cpu(
                                    &tm_ids, states, symbols, max_steps, rounds, &dyn_payoff,
                                )
                            }
                        };
                    } else {
                        scores = tournament::run_tournament_cpu(
                            &tm_ids, states, symbols, max_steps, rounds, &dyn_payoff,
                        );
                    }
                }
                #[cfg(not(all(target_os = "macos", feature = "metal")))]
                {
                    if use_gpu {
                        eprintln!("  Metal not compiled in, using CPU");
                    }
                    scores = tournament::run_tournament_cpu(
                        &tm_ids, states, symbols, max_steps, rounds, &dyn_payoff,
                    );
                }

                let output = tournament::build_output(
                    &tm_ids, scores, rounds, &game, states, symbols,
                );

                eprintln!(
                    "  done: {} pairs scored",
                    output.num_pairs
                );

                tournament::write_output(&output);
            } else {
                eprintln!("Error: must provide --ids/--ids-file or --strategies/--strategies-file");
                std::process::exit(1);
            }
        }
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── digits_in_base ──────────────────────────────────────────────────

    #[test]
    fn digits_zero() {
        assert_eq!(digits_in_base(0, 2), vec![0]);
    }

    #[test]
    fn digits_binary() {
        // 5 = 101 in base 2
        assert_eq!(digits_in_base(5, 2), vec![1, 0, 1]);
    }

    #[test]
    fn digits_base10() {
        assert_eq!(digits_in_base(42, 10), vec![4, 2]);
    }

    #[test]
    fn digits_roundtrip() {
        // digits → value should round-trip
        for val in [0, 1, 7, 15, 255, 1000] {
            let d = digits_in_base(val, 2);
            let mut reconstructed = 0u64;
            for &dig in &d {
                reconstructed = reconstructed * 2 + dig as u64;
            }
            assert_eq!(reconstructed, val);
        }
    }

    // ── tm_max_index ────────────────────────────────────────────────────

    #[test]
    fn max_index_2_2() {
        // (2*2*2)^(2*2) - 1 = 8^4 - 1 = 4095
        assert_eq!(tm_max_index(2, 2), Some(4095));
    }

    #[test]
    fn max_index_3_2() {
        // (2*3*2)^(3*2) - 1 = 12^6 - 1 = 2985983
        assert_eq!(tm_max_index(3, 2), Some(2985983));
    }

    #[test]
    fn max_index_4_2() {
        // (2*4*2)^(4*2) - 1 = 16^8 - 1 = 4294967295
        assert_eq!(tm_max_index(4, 2), Some(4294967295));
    }

    #[test]
    fn max_index_1_1() {
        // (2*1*1)^(1*1) - 1 = 2^1 - 1 = 1
        assert_eq!(tm_max_index(1, 1), Some(1));
    }

    // ── decode_tm ───────────────────────────────────────────────────────

    #[test]
    fn decode_tm_zero_all_left() {
        // Rule 0: all transitions should write 0, move Left, go to state 1
        let trans = decode_tm(0, 2, 2);
        assert_eq!(trans.len(), 4); // 2 states * 2 symbols
        for t in &trans {
            assert_eq!(t.write, 0);
            assert!(!t.move_right); // Left
            assert_eq!(t.next, 1);
        }
    }

    #[test]
    fn decode_tm_table_size() {
        assert_eq!(decode_tm(0, 3, 2).len(), 6);  // 3*2
        assert_eq!(decode_tm(0, 2, 3).len(), 6);  // 2*3
        assert_eq!(decode_tm(0, 4, 2).len(), 8);  // 4*2
        assert_eq!(decode_tm(0, 1, 2).len(), 2);  // 1*2
    }

    #[test]
    fn decode_tm_next_state_range() {
        // All next-states must be in [1, states]
        for id in 0..100 {
            let trans = decode_tm(id, 2, 2);
            for t in &trans {
                assert!(t.next >= 1 && t.next <= 2,
                        "TM {}: next={} out of [1,2]", id, t.next);
            }
        }
        for id in 0..100 {
            let trans = decode_tm(id, 3, 2);
            for t in &trans {
                assert!(t.next >= 1 && t.next <= 3,
                        "TM {}: next={} out of [1,3]", id, t.next);
            }
        }
    }

    #[test]
    fn decode_tm_write_symbol_range() {
        // All write symbols must be in [0, symbols)
        for id in 0..100 {
            let trans = decode_tm(id, 2, 2);
            for t in &trans {
                assert!(t.write < 2, "TM {}: write={}", id, t.write);
            }
        }
    }

    #[test]
    fn decode_tm_max_index_valid() {
        // The max-index TM should decode without panic
        let trans = decode_tm(4095, 2, 2);
        assert_eq!(trans.len(), 4);
        let trans = decode_tm(2985983, 3, 2);
        assert_eq!(trans.len(), 6);
    }

    // ── run_tm ──────────────────────────────────────────────────────────

    #[test]
    fn run_tm_zero_never_halts() {
        // TM 0: all transitions go Left from state 1. Head drifts left
        // inserting blanks forever → should not halt.
        let trans = decode_tm(0, 2, 2);
        let (halted, _, _) = run_tm(&trans, 2, 0, 100);
        assert!(!halted);
    }

    #[test]
    fn run_tm_64_always_halts() {
        // TM 64 in (2,2) should halt on all small inputs
        let trans = decode_tm(64, 2, 2);
        for input in 0..16 {
            let (halted, _, _) = run_tm(&trans, 2, input, 500);
            assert!(halted, "TM 64 failed to halt on input {}", input);
        }
    }

    #[test]
    fn run_tm_64_outputs_zero() {
        // TM 64 always outputs 0 (cooperate) on all depth-2 inputs
        let trans = decode_tm(64, 2, 2);
        for input in 0..16 {
            let (halted, out, _) = run_tm(&trans, 2, input, 500);
            assert!(halted);
            assert_eq!(out, 0, "TM 64, input {}: expected 0 got {}", input, out);
        }
    }

    #[test]
    fn run_tm_323_mixed_outputs() {
        // TM 323 in (2,2): on depth-1 inputs (0..3), outputs should be
        // {0→0, 1→1, 2→0, 3→0} based on our earlier test.
        let trans = decode_tm(323, 2, 2);
        let expected = [(0, 0), (1, 1), (2, 0), (3, 0)];
        for (input, exp_out) in expected {
            let (halted, out, _) = run_tm(&trans, 2, input, 500);
            assert!(halted, "TM 323 didn't halt on input {}", input);
            assert_eq!(out, exp_out,
                       "TM 323, input {}: expected {} got {}", input, exp_out, out);
        }
    }

    #[test]
    fn run_tm_respects_max_steps() {
        // A non-halting TM should return (false, _, max_steps)
        let trans = decode_tm(0, 2, 2);
        let (halted, _, steps) = run_tm(&trans, 2, 0, 50);
        assert!(!halted);
        assert_eq!(steps, 50);
    }

    // ── test_halting ────────────────────────────────────────────────────

    #[test]
    fn test_halting_tm0_fails() {
        let trans = decode_tm(0, 2, 2);
        assert!(test_halting(&trans, 2, 500, 1).is_none());
    }

    #[test]
    fn test_halting_tm64_passes() {
        let trans = decode_tm(64, 2, 2);
        let result = test_halting(&trans, 2, 500, 4);
        assert!(result.is_some());
        // Should have 4+16+64+256 = 340 outputs
        assert_eq!(result.unwrap().len(), 4 + 16 + 64 + 256);
    }

    #[test]
    fn test_halting_depth_1_input_count() {
        // depth=1 → test inputs 0..3 for k=2 → 4 inputs
        let trans = decode_tm(64, 2, 2);
        let result = test_halting(&trans, 2, 500, 1).unwrap();
        assert_eq!(result.len(), 4);
    }

    #[test]
    fn test_halting_depth_2_input_count() {
        // depth=2 → 4 + 16 = 20 inputs for k=2
        let trans = decode_tm(64, 2, 2);
        let result = test_halting(&trans, 2, 500, 2).unwrap();
        assert_eq!(result.len(), 20);
    }

    // ── Aggregate / statistical tests ───────────────────────────────────

    #[test]
    fn tm22_halting_count_in_expected_range() {
        // We know ~41% of TM(2,2) halt at depth 4.
        // Check full enumeration is in [1400, 1900].
        let mut count = 0u32;
        for id in 0..=4095u64 {
            let trans = decode_tm(id, 2, 2);
            if test_halting(&trans, 2, 500, 4).is_some() {
                count += 1;
            }
        }
        assert!(count >= 1400 && count <= 1900,
                "TM(2,2) halting count {} outside [1400,1900]", count);
    }

    #[test]
    fn no_halting_tm_below_64() {
        // TMs 0..63 in (2,2) should all fail to halt (empirical observation)
        for id in 0..64u64 {
            let trans = decode_tm(id, 2, 2);
            assert!(test_halting(&trans, 2, 500, 1).is_none(),
                    "TM {} unexpectedly halts", id);
        }
    }

    // ── Cross-check with nit-games encoding ─────────────────────────────

    #[test]
    fn decode_matches_nit_games_base_formula() {
        // In nit-games: base = symbols * states * 2
        // For (2,2): base = 8
        // Extraction order: state goes high-to-low, read goes low-to-high
        //   iteration: (state=2,read=0), (state=2,read=1), (state=1,read=0), (state=1,read=1)
        // id=1: first extracted (state=2,read=0) gets digit=1%8=1, rest=0
        // digit=1: move_idx=1(R), write=(1/2)%2=0, next=(1/4)+1=1
        let trans = decode_tm(1, 2, 2);
        // state2, read0 = index 2
        assert!(trans[2].move_right);
        assert_eq!(trans[2].write, 0);
        assert_eq!(trans[2].next, 1);
        // All other transitions should be default (digit=0 → L, write=0, next=1)
        for &i in &[0, 1, 3] {
            assert!(!trans[i].move_right);
            assert_eq!(trans[i].write, 0);
            assert_eq!(trans[i].next, 1);
        }
    }

    #[test]
    fn decode_id8_second_digit() {
        // id=8, base=8: first extracted (state2,read0): 8%8=0 → default
        // then code=8/8=1 → (state2,read1): 1%8=1 → R, write=0, next=1
        let trans = decode_tm(8, 2, 2);
        // state2, read1 = index 3
        assert!(trans[3].move_right);
        assert_eq!(trans[3].write, 0);
        assert_eq!(trans[3].next, 1);
    }

    // ── Edge cases ──────────────────────────────────────────────────────

    #[test]
    fn run_tm_input_zero() {
        // Input 0 → tape [0], head at 0
        let trans = decode_tm(64, 2, 2);
        let (halted, _, _) = run_tm(&trans, 2, 0, 500);
        assert!(halted);
    }

    #[test]
    fn run_tm_large_input() {
        // TM 64 should still halt on a large input
        let trans = decode_tm(64, 2, 2);
        let (halted, _, _) = run_tm(&trans, 2, 65535, 5000);
        assert!(halted);
    }

    #[test]
    fn checked_pow_basic() {
        assert_eq!(checked_pow(2, 0), Some(1));
        assert_eq!(checked_pow(2, 10), Some(1024));
        assert_eq!(checked_pow(8, 4), Some(4096));
        assert_eq!(checked_pow(12, 6), Some(2985984));
    }

    #[test]
    fn checked_pow_overflow() {
        // 2^128 should overflow u128
        assert_eq!(checked_pow(2, 128), None);
    }
}
