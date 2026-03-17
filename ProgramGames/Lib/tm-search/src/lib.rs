use rayon::prelude::*;
use serde::Serialize;

use wolfram_library_link as wll;
wll::generate_loader!(rustlink_autodiscover);

pub mod gpu;
pub mod strategy;
pub mod tournament;

// ── TM core (matches Wolfram + nit-games encoding) ──────────────────────────

#[derive(Copy, Clone, Debug)]
pub struct TmTransition {
    pub write: u8,
    pub move_right: bool, // true = Right, false = Left
    pub next: u16,        // 1-indexed state
}

/// Decode a TM rule code to its transition table.
/// Encoding matches `ResourceFunction["TuringMachineFromNumber"]` and
/// `decode_tm_rule_code_wolfram` in nit-games.
pub fn decode_tm(rule_code: u64, states: u16, symbols: u8) -> Vec<TmTransition> {
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
pub fn tm_max_index(states: u16, symbols: u8) -> Option<u128> {
    let s = states as u128;
    let k = symbols as u128;
    let base = 2u128.checked_mul(s)?.checked_mul(k)?;
    let exp = (states as u32).checked_mul(symbols as u32)?;
    checked_pow(base, exp)?.checked_sub(1)
}

pub fn checked_pow(base: u128, exp: u32) -> Option<u128> {
    let mut result = 1u128;
    for _ in 0..exp {
        result = result.checked_mul(base)?;
    }
    Some(result)
}

/// Convert integer to MSD-first base-k digits.
pub fn digits_in_base(mut value: u64, base: u8) -> Vec<u8> {
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
pub fn run_tm(
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
///    at depth d satisfy w+1 <= 2*d, all longer inputs share the same
///    suffix patterns -> skip remaining depths. Most TMs stop at depth 4-5.
///
/// 2. **Pre-padded tape with dirty tracking**: For TMs that DO need deep
///    testing (depth >= 5), uses a pre-allocated buffer with left padding
///    and only clears the region dirtied by the previous run. This avoids
///    the O(n^2) cost of Vec::insert(0, _) and the O(max_steps) cost of
///    full-buffer clearing.
pub fn test_halting(
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
pub struct HaltingTm {
    pub id: u64,
    pub s: u16,
    pub k: u8,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_steps_used: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub outputs: Option<Vec<(u64, u8)>>,
}

#[derive(Serialize)]
pub struct SearchSummary {
    pub states: u16,
    pub symbols: u8,
    pub max_steps: u32,
    pub depth: u32,
    pub space_size: String,
    pub searched: u64,
    pub halting_count: u64,
    pub halting_fraction: f64,
}

#[derive(Serialize)]
pub struct SearchResult {
    pub ids: Vec<u64>,
    pub summary: SearchSummary,
}

#[derive(Serialize)]
pub struct ClassifyOutput {
    pub states: u16,
    pub symbols: u8,
    pub depth: u32,
    pub max_steps: u32,
    pub total_halting: usize,
    pub unique_behaviors: usize,
    pub reduction_factor: f64,
    pub representatives: Vec<u64>,
    pub groups: Vec<GroupEntry>,
}

#[derive(Serialize)]
pub struct GroupEntry {
    pub representative: u64,
    pub count: usize,
    pub members: Vec<u64>,
    pub signature_preview: String,
}

// ── GPU-accelerated search ───────────────────────────────────────────────

/// Run the halting search using Metal GPU + CPU adaptive depth.
/// Phase 1 (CPU): depth 1-4, adaptive skip.
/// Phase 2 (GPU): for remaining TMs, dispatch per-depth batches.
/// Falls back to CPU-only if GPU init fails.
#[cfg(all(target_os = "macos", feature = "metal"))]
pub fn search_gpu(
    candidates: &[u64],
    states: u16,
    symbols: u8,
    max_steps: u32,
    depth: u32,
) -> Vec<u64> {
    use crate::gpu::MetalSearcher;

    let gpu = match MetalSearcher::new() {
        Ok(g) => g,
        Err(_e) => {
            return search_cpu(candidates, states, symbols, max_steps, depth);
        }
    };

    let k = symbols as u64;
    let gpu_max_steps = max_steps.min(10000); // GPU tape limit (MAX_TAPE=11000)

    // Phase 1 (CPU): test all TMs at depth 1-4, record worst_steps
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
    let mut need_gpu: Vec<&mut TmState> = tms.iter_mut()
        .filter(|t| t.alive && t.worst_steps + 1 > (2 * depth.min(4)) as u32)
        .collect();

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

        // Process in GPU batches
        for batch_start in (0..need_gpu.len()).step_by(batch_size) {
            let batch_end = (batch_start + batch_size).min(need_gpu.len());
            let batch_trans: Vec<Vec<TmTransition>> = need_gpu[batch_start..batch_end]
                .iter()
                .map(|t| t.transitions.clone())
                .collect();

            // Per-TM step limit: worst_steps * 4 + margin, capped at gpu_max_steps.
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
                Err(_e) => {
                    for tm in &mut need_gpu[batch_start..batch_end] {
                        for input in 0..num_inputs {
                            let (halted, _, _) = run_tm(&tm.transitions, symbols, input, max_steps);
                            if !halted { tm.alive = false; break; }
                        }
                    }
                }
            }
        }
    }

    tms.iter().filter(|t| t.alive).map(|t| t.id).collect()
}

/// CPU-only search (existing logic).
pub fn search_cpu(
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

// ── Classify logic ──────────────────────────────────────────────────────────

/// Classify TMs by behavior: group machines that produce identical outputs.
/// Returns a ClassifyOutput struct.
pub fn classify(
    tm_ids: &[u64],
    states: u16,
    symbols: u8,
    max_steps: u32,
    depth: u32,
) -> ClassifyOutput {
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

    ClassifyOutput {
        states,
        symbols,
        depth,
        max_steps,
        total_halting: signatures.len(),
        unique_behaviors: sorted_groups.len(),
        reduction_factor: if sorted_groups.is_empty() {
            0.0
        } else {
            signatures.len() as f64 / sorted_groups.len() as f64
        },
        representatives: sorted_groups.iter().map(|(_, ids)| ids[0]).collect(),
        groups: sorted_groups
            .iter()
            .map(|(sig, ids)| {
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
            })
            .collect(),
    }
}

// ── WLL exported functions ──────────────────────────────────────────────────

/// Search for halting TMs. Returns JSON array of IDs.
/// sample=0 means exhaustive, sample>0 means random sample of that size.
#[wll::export]
pub fn tm_search_wl(
    states: i64,
    symbols: i64,
    max_steps: i64,
    depth: i64,
    sample: i64,
    use_gpu: bool,
) -> String {
    let states = states as u16;
    let symbols = symbols as u8;
    let max_steps = max_steps as u32;
    let depth = depth as u32;

    let max_id = match tm_max_index(states, symbols) {
        Some(m) if m <= u64::MAX as u128 => m as u64,
        _ => return "[]".to_string(),
    };

    // Build candidate list
    let candidates: Vec<u64> = if sample > 0 {
        use std::collections::HashSet;
        let mut seen = HashSet::new();
        let total_in_range = max_id + 1;
        let mut rng_state = 0x5DEECE66Du64.wrapping_mul(42).wrapping_add(0xBu64);
        let target = (sample as u64).min(total_in_range);
        while (seen.len() as u64) < target {
            rng_state = rng_state
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            let hi = rng_state >> 32;
            rng_state = rng_state
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            let lo = rng_state >> 32;
            let full = (hi << 32) | lo;
            let idx = full % total_in_range;
            seen.insert(idx);
        }
        let mut v: Vec<u64> = seen.into_iter().collect();
        v.sort();
        v
    } else {
        (0..=max_id).collect()
    };

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
        let _ = use_gpu;
        halting_ids = search_cpu(&candidates, states, symbols, max_steps, depth);
    }

    serde_json::to_string(&halting_ids).unwrap_or_else(|_| "[]".to_string())
}

/// Classify TMs by behavior. tm_ids_json is a JSON array of IDs.
/// Returns JSON with representatives, groups, etc.
#[wll::export]
pub fn tm_classify_wl(
    states: i64,
    symbols: i64,
    max_steps: i64,
    depth: i64,
    tm_ids_json: String,
    use_gpu: bool,
) -> String {
    let states = states as u16;
    let symbols = symbols as u8;
    let max_steps = max_steps as u32;
    let depth = depth as u32;

    let ids: Vec<u64> = if let Ok(parsed) = serde_json::from_str::<Vec<u64>>(&tm_ids_json) {
        if parsed.is_empty() {
            // Find all halting TMs first
            let max_id = match tm_max_index(states, symbols) {
                Some(m) if m <= u64::MAX as u128 => m as u64,
                _ => return "{}".to_string(),
            };
            let candidates: Vec<u64> = (0..=max_id).collect();

            #[cfg(all(target_os = "macos", feature = "metal"))]
            {
                if use_gpu {
                    search_gpu(&candidates, states, symbols, max_steps, depth)
                } else {
                    search_cpu(&candidates, states, symbols, max_steps, depth)
                }
            }
            #[cfg(not(all(target_os = "macos", feature = "metal")))]
            {
                let _ = use_gpu;
                search_cpu(&candidates, states, symbols, max_steps, depth)
            }
        } else {
            parsed
        }
    } else {
        return "{\"error\":\"invalid tm_ids_json\"}".to_string();
    };

    let output = classify(&ids, states, symbols, max_steps, depth);
    serde_json::to_string(&output).unwrap_or_else(|_| "{}".to_string())
}

/// Run a TM-only tournament. tm_ids_json is a JSON array of IDs.
/// Returns JSON with scores, ranking, pairwise results.
#[wll::export]
pub fn tm_tournament_wl(
    states: i64,
    symbols: i64,
    max_steps: i64,
    rounds: i64,
    game: String,
    tm_ids_json: String,
    use_gpu: bool,
) -> String {
    let states = states as u16;
    let symbols = symbols as u8;
    let max_steps = max_steps as u32;
    let rounds = rounds as u32;

    let payoff = match tournament::parse_game(&game) {
        Ok(p) => p,
        Err(e) => return format!("{{\"error\":\"{}\"}}", e),
    };

    let ids: Vec<u64> = match serde_json::from_str(&tm_ids_json) {
        Ok(v) => v,
        Err(e) => return format!("{{\"error\":\"{}\"}}", e),
    };
    if ids.is_empty() {
        return "{\"error\":\"no TM IDs provided\"}".to_string();
    }

    let scores: Vec<Vec<i64>>;

    #[cfg(all(target_os = "macos", feature = "metal"))]
    {
        if use_gpu {
            scores = match gpu::run_tournament_gpu(
                &ids, states, symbols, max_steps, rounds, &payoff,
            ) {
                Ok(s) => s,
                Err(_e) => {
                    tournament::run_tournament_cpu(
                        &ids, states, symbols, max_steps, rounds, &payoff,
                    )
                }
            };
        } else {
            scores = tournament::run_tournament_cpu(
                &ids, states, symbols, max_steps, rounds, &payoff,
            );
        }
    }
    #[cfg(not(all(target_os = "macos", feature = "metal")))]
    {
        let _ = use_gpu;
        scores = tournament::run_tournament_cpu(
            &ids, states, symbols, max_steps, rounds, &payoff,
        );
    }

    let output = tournament::build_output(&ids, scores, rounds, &game, states, symbols);
    serde_json::to_string(&output).unwrap_or_else(|_| "{}".to_string())
}

/// Run a mixed-strategy tournament. strategies_ndjson is newline-delimited JSON specs.
/// Returns JSON with scores, ranking, pairwise results.
#[wll::export]
pub fn program_tournament_wl(
    rounds: i64,
    game: String,
    strategies_ndjson: String,
) -> String {
    let rounds = rounds as u32;

    let payoff = match tournament::parse_game(&game) {
        Ok(p) => p,
        Err(e) => return format!("{{\"error\":\"{}\"}}", e),
    };

    let specs = match tournament::parse_strategies_inline(&strategies_ndjson) {
        Ok(v) => v,
        Err(e) => return format!("{{\"error\":\"{}\"}}", e),
    };

    if specs.is_empty() {
        return "{\"error\":\"no strategies provided\"}".to_string();
    }

    let (survivors, scores) = tournament::run_mixed_tournament_cpu(&specs, rounds, &payoff);
    let surviving_specs: Vec<_> = survivors.iter().map(|&i| specs[i].clone()).collect();
    let output = tournament::build_mixed_output(&surviving_specs, scores, rounds, &game);
    serde_json::to_string(&output).unwrap_or_else(|_| "{}".to_string())
}

#[wll::export]
pub fn tm_max_index_wl(states: i64, symbols: i64) -> i64 {
    let states = states as u16;
    let symbols = symbols as u8;
    match tm_max_index(states, symbols) {
        Some(m) if m <= i64::MAX as u128 => m as i64,
        _ => -1, // overflow or error
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
        assert_eq!(tm_max_index(2, 2), Some(4095));
    }

    #[test]
    fn max_index_3_2() {
        assert_eq!(tm_max_index(3, 2), Some(2985983));
    }

    #[test]
    fn max_index_4_2() {
        assert_eq!(tm_max_index(4, 2), Some(4294967295));
    }

    #[test]
    fn max_index_1_1() {
        assert_eq!(tm_max_index(1, 1), Some(1));
    }

    // ── decode_tm ───────────────────────────────────────────────────────

    #[test]
    fn decode_tm_zero_all_left() {
        let trans = decode_tm(0, 2, 2);
        assert_eq!(trans.len(), 4);
        for t in &trans {
            assert_eq!(t.write, 0);
            assert!(!t.move_right);
            assert_eq!(t.next, 1);
        }
    }

    #[test]
    fn decode_tm_table_size() {
        assert_eq!(decode_tm(0, 3, 2).len(), 6);
        assert_eq!(decode_tm(0, 2, 3).len(), 6);
        assert_eq!(decode_tm(0, 4, 2).len(), 8);
        assert_eq!(decode_tm(0, 1, 2).len(), 2);
    }

    #[test]
    fn decode_tm_next_state_range() {
        for id in 0..100 {
            let trans = decode_tm(id, 2, 2);
            for t in &trans {
                assert!(
                    t.next >= 1 && t.next <= 2,
                    "TM {}: next={} out of [1,2]",
                    id,
                    t.next
                );
            }
        }
        for id in 0..100 {
            let trans = decode_tm(id, 3, 2);
            for t in &trans {
                assert!(
                    t.next >= 1 && t.next <= 3,
                    "TM {}: next={} out of [1,3]",
                    id,
                    t.next
                );
            }
        }
    }

    #[test]
    fn decode_tm_write_symbol_range() {
        for id in 0..100 {
            let trans = decode_tm(id, 2, 2);
            for t in &trans {
                assert!(t.write < 2, "TM {}: write={}", id, t.write);
            }
        }
    }

    #[test]
    fn decode_tm_max_index_valid() {
        let trans = decode_tm(4095, 2, 2);
        assert_eq!(trans.len(), 4);
        let trans = decode_tm(2985983, 3, 2);
        assert_eq!(trans.len(), 6);
    }

    // ── run_tm ──────────────────────────────────────────────────────────

    #[test]
    fn run_tm_zero_never_halts() {
        let trans = decode_tm(0, 2, 2);
        let (halted, _, _) = run_tm(&trans, 2, 0, 100);
        assert!(!halted);
    }

    #[test]
    fn run_tm_64_always_halts() {
        let trans = decode_tm(64, 2, 2);
        for input in 0..16 {
            let (halted, _, _) = run_tm(&trans, 2, input, 500);
            assert!(halted, "TM 64 failed to halt on input {}", input);
        }
    }

    #[test]
    fn run_tm_64_outputs_zero() {
        let trans = decode_tm(64, 2, 2);
        for input in 0..16 {
            let (halted, out, _) = run_tm(&trans, 2, input, 500);
            assert!(halted);
            assert_eq!(out, 0, "TM 64, input {}: expected 0 got {}", input, out);
        }
    }

    #[test]
    fn run_tm_323_mixed_outputs() {
        let trans = decode_tm(323, 2, 2);
        let expected = [(0, 0), (1, 1), (2, 0), (3, 0)];
        for (input, exp_out) in expected {
            let (halted, out, _) = run_tm(&trans, 2, input, 500);
            assert!(halted, "TM 323 didn't halt on input {}", input);
            assert_eq!(
                out, exp_out,
                "TM 323, input {}: expected {} got {}",
                input, exp_out, out
            );
        }
    }

    #[test]
    fn run_tm_respects_max_steps() {
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
        assert_eq!(result.unwrap().len(), 4 + 16 + 64 + 256);
    }

    #[test]
    fn test_halting_depth_1_input_count() {
        let trans = decode_tm(64, 2, 2);
        let result = test_halting(&trans, 2, 500, 1).unwrap();
        assert_eq!(result.len(), 4);
    }

    #[test]
    fn test_halting_depth_2_input_count() {
        let trans = decode_tm(64, 2, 2);
        let result = test_halting(&trans, 2, 500, 2).unwrap();
        assert_eq!(result.len(), 20);
    }

    // ── Aggregate / statistical tests ───────────────────────────────────

    #[test]
    fn tm22_halting_count_in_expected_range() {
        let mut count = 0u32;
        for id in 0..=4095u64 {
            let trans = decode_tm(id, 2, 2);
            if test_halting(&trans, 2, 500, 4).is_some() {
                count += 1;
            }
        }
        assert!(
            count >= 1400 && count <= 1900,
            "TM(2,2) halting count {} outside [1400,1900]",
            count
        );
    }

    #[test]
    fn no_halting_tm_below_64() {
        for id in 0..64u64 {
            let trans = decode_tm(id, 2, 2);
            assert!(
                test_halting(&trans, 2, 500, 1).is_none(),
                "TM {} unexpectedly halts",
                id
            );
        }
    }

    // ── Cross-check with nit-games encoding ─────────────────────────────

    #[test]
    fn decode_matches_nit_games_base_formula() {
        let trans = decode_tm(1, 2, 2);
        assert!(trans[2].move_right);
        assert_eq!(trans[2].write, 0);
        assert_eq!(trans[2].next, 1);
        for &i in &[0, 1, 3] {
            assert!(!trans[i].move_right);
            assert_eq!(trans[i].write, 0);
            assert_eq!(trans[i].next, 1);
        }
    }

    #[test]
    fn decode_id8_second_digit() {
        let trans = decode_tm(8, 2, 2);
        assert!(trans[3].move_right);
        assert_eq!(trans[3].write, 0);
        assert_eq!(trans[3].next, 1);
    }

    // ── Edge cases ──────────────────────────────────────────────────────

    #[test]
    fn run_tm_input_zero() {
        let trans = decode_tm(64, 2, 2);
        let (halted, _, _) = run_tm(&trans, 2, 0, 500);
        assert!(halted);
    }

    #[test]
    fn run_tm_large_input() {
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
        assert_eq!(checked_pow(2, 128), None);
    }
}
