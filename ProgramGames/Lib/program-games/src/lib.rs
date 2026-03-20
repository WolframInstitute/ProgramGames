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
/// Phase 1 (CPU): depth 1-4, adaptive skip, using pre-padded tape.
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

    // Phase 1 (CPU): test all TMs at depth 1-4, record worst_steps.
    // Uses pre-padded tape to avoid O(n) Vec::insert.
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
            let tape_cap = max_steps as usize + 20;
            let mut tape = vec![0u8; tape_cap];
            for r in 1..=depth.min(4) {
                if r > 1 && worst + 1 <= (2 * (r - 1)) as u32 {
                    break;
                }
                let max_input = k.checked_pow(2 * r).unwrap_or(u64::MAX);
                let input_width = (2 * r) as usize;
                let left_pad = tape_cap - input_width;
                for input in 0..max_input {
                    // Clear and setup tape
                    for b in tape[..tape_cap].iter_mut() { *b = 0; }
                    let mut val = input;
                    for i in (0..input_width).rev() {
                        tape[left_pad + i] = (val % k) as u8;
                        val /= k;
                    }
                    // Run TM with pre-padded tape
                    let mut head = tape_cap - 1;
                    let mut state: u16 = 1;
                    let mut halted = false;
                    let mut steps = 0u32;
                    for step in 0..max_steps {
                        let read = tape[head];
                        let idx = (state as usize - 1) * (symbols as usize) + (read as usize);
                        let t = transitions[idx];
                        tape[head] = t.write;
                        if t.move_right {
                            if head + 1 == tape_cap {
                                halted = true;
                                steps = step + 1;
                                break;
                            }
                            head += 1;
                        } else if head == 0 {
                            break;
                        } else {
                            head -= 1;
                        }
                        state = t.next;
                        if state == 0 { break; }
                    }
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

    eprintln!("  GPU phase 1 done: {} need deeper testing", need_gpu.len());

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

        eprintln!("  GPU depth {}: {} TMs x {} inputs", r, need_gpu.len(), num_inputs);

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
/// Returns a ClassifyOutput struct. When `use_gpu` is true, tries Metal GPU
/// acceleration. GPU is used for k=2 (sig_len=340, lightweight threads);
/// CPU is preferred for k≥3 (sig_len≥7380, heavy per-thread work).
pub fn classify(
    tm_ids: &[u64],
    states: u16,
    symbols: u8,
    max_steps: u32,
    depth: u32,
    use_gpu: bool,
) -> ClassifyOutput {
    let classify_depth = depth.min(4);
    let sig_len = gpu::compute_tm_sig_len(symbols, classify_depth);

    // GPU is beneficial when per-thread work is lightweight (small sig_len).
    // For k≥3, sig_len≥7380 and each thread does thousands of TM runs,
    // which causes register spilling and poor GPU occupancy.
    let gpu_worthwhile = use_gpu && sig_len <= 1000;

    // Try GPU classification
    let signatures: Vec<(u64, Vec<u8>)>;

    #[cfg(all(target_os = "macos", feature = "metal"))]
    {
        if gpu_worthwhile {
            match gpu::classify_tm_gpu(tm_ids, states, symbols, max_steps, classify_depth) {
                Ok((flat_sigs, sl)) => {
                    eprintln!("  GPU classify: {} TMs, sig_len={}", tm_ids.len(), sl);
                    signatures = tm_ids.iter().enumerate().map(|(i, &id)| {
                        let start = i * sl;
                        let sig = flat_sigs[start..start + sl].to_vec();
                        (id, sig)
                    }).collect();
                }
                Err(e) => {
                    eprintln!("  GPU classify failed ({}), falling back to CPU", e);
                    signatures = classify_cpu_signatures(tm_ids, states, symbols, max_steps, classify_depth);
                }
            }
        } else {
            signatures = classify_cpu_signatures(tm_ids, states, symbols, max_steps, classify_depth);
        }
    }
    #[cfg(not(all(target_os = "macos", feature = "metal")))]
    {
        let _ = use_gpu;
        let _ = sig_len;
        let _ = gpu_worthwhile;
        signatures = classify_cpu_signatures(tm_ids, states, symbols, max_steps, classify_depth);
    }

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

/// CPU classification: compute behavioral signatures using optimized pre-padded tape.
fn classify_cpu_signatures(
    tm_ids: &[u64],
    states: u16,
    symbols: u8,
    max_steps: u32,
    classify_depth: u32,
) -> Vec<(u64, Vec<u8>)> {
    let k = symbols as u64;
    tm_ids
        .par_iter()
        .map(|&id| {
            let trans = decode_tm(id, states, symbols);
            let tape_cap = max_steps as usize + 20;
            let mut tape = vec![0u8; tape_cap];
            let mut sig = Vec::new();
            for r in 1..=classify_depth {
                let max_input = k.pow(2 * r);
                let input_width = (2 * r) as usize;
                let left_pad = tape_cap - input_width;
                for input in 0..max_input {
                    // Clear dirty region (full clear for correctness)
                    for b in tape[..tape_cap].iter_mut() { *b = 0; }
                    // Decode input digits
                    let mut val = input;
                    for i in (0..input_width).rev() {
                        tape[left_pad + i] = (val % k) as u8;
                        val /= k;
                    }
                    // Run TM with pre-padded tape
                    let mut head = tape_cap - 1;
                    let mut state: u16 = 1;
                    let mut output = 1u8; // non-halting default
                    for _step in 0..max_steps {
                        let read = tape[head];
                        let idx = (state as usize - 1) * (symbols as usize) + (read as usize);
                        let t = trans[idx];
                        tape[head] = t.write;
                        if t.move_right {
                            if head + 1 == tape_cap {
                                output = tape[tape_cap - 1] % 2;
                                break;
                            }
                            head += 1;
                        } else if head == 0 {
                            break;
                        } else {
                            head -= 1;
                        }
                        state = t.next;
                        if state == 0 { break; }
                    }
                    sig.push(output);
                }
            }
            (id, sig)
        })
        .collect()
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

    let output = classify(&ids, states, symbols, max_steps, depth, use_gpu);
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

    let dyn_payoff = match tournament::parse_game_dyn(&game) {
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

    let survivors: Vec<usize>;
    let scores: Vec<Vec<i64>>;

    #[cfg(all(target_os = "macos", feature = "metal"))]
    {
        if use_gpu {
            match gpu::run_tournament_gpu(
                &ids, states, symbols, max_steps, rounds,
                dyn_payoff.num_actions as u32, &dyn_payoff.entries,
            ) {
                Ok((s, sc)) => { survivors = s; scores = sc; }
                Err(_e) => {
                    let (s, sc) = tournament::run_tournament_cpu(
                        &ids, states, symbols, max_steps, rounds, &dyn_payoff,
                    );
                    survivors = s; scores = sc;
                }
            };
        } else {
            let (s, sc) = tournament::run_tournament_cpu(
                &ids, states, symbols, max_steps, rounds, &dyn_payoff,
            );
            survivors = s; scores = sc;
        }
    }
    #[cfg(not(all(target_os = "macos", feature = "metal")))]
    {
        let _ = use_gpu;
        let (s, sc) = tournament::run_tournament_cpu(
            &ids, states, symbols, max_steps, rounds, &dyn_payoff,
        );
        survivors = s; scores = sc;
    }

    let survivor_ids: Vec<u64> = survivors.iter().map(|&i| ids[i]).collect();

    let output = tournament::build_output(&survivor_ids, scores, rounds, &game, states, symbols);
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

    let payoff = match tournament::parse_game_dyn(&game) {
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

/// Run a GPU-accelerated CA tournament. Falls back to CPU if Metal unavailable.
#[wll::export]
pub fn ca_tournament_wl(
    k: i64,
    r_numer: i64,
    r_denom: i64,
    t: i64,
    rounds: i64,
    game: String,
    rules_json: String,
    use_gpu: bool,
) -> String {
    let k = k as u8;
    let r_f = r_numer as f32 / r_denom as f32;
    let two_r = (2.0 * r_f).round() as u32;
    let t = t as u32;
    let rounds = rounds as u32;

    let dyn_payoff = match tournament::parse_game_dyn(&game) {
        Ok(p) => p,
        Err(e) => return format!("{{\"error\":\"{}\"}}", e),
    };
    let num_actions = dyn_payoff.num_actions as u8;

    let rule_ids: Vec<u64> = match serde_json::from_str(&rules_json) {
        Ok(v) => v,
        Err(e) => return format!("{{\"error\":\"{}\"}}", e),
    };
    if rule_ids.is_empty() {
        return "{\"error\":\"no CA rules provided\"}".to_string();
    }

    let scores: Vec<Vec<i64>>;
    let used_gpu: bool;

    #[cfg(all(target_os = "macos", feature = "metal"))]
    {
        if use_gpu {
            match gpu::run_ca_tournament_gpu(
                &rule_ids, k, two_r, t, rounds,
                dyn_payoff.num_actions as u32, &dyn_payoff.entries,
            ) {
                Ok(s) => {
                    scores = s;
                    used_gpu = true;
                }
                Err(_e) => {
                    scores = run_ca_tournament_cpu(
                        &rule_ids, k, two_r, t, rounds, &dyn_payoff, num_actions,
                    );
                    used_gpu = false;
                }
            };
        } else {
            scores = run_ca_tournament_cpu(
                &rule_ids, k, two_r, t, rounds, &dyn_payoff, num_actions,
            );
            used_gpu = false;
        }
    }
    #[cfg(not(all(target_os = "macos", feature = "metal")))]
    {
        let _ = use_gpu;
        scores = run_ca_tournament_cpu(
            &rule_ids, k, two_r, t, rounds, &dyn_payoff, num_actions,
        );
        used_gpu = false;
    }

    // Build output using mixed-output format with CA specs
    let specs: Vec<strategy::StrategySpec> = rule_ids
        .iter()
        .map(|&rule| strategy::StrategySpec::Ca { rule, k, r: r_f, t, num_actions })
        .collect();
    let output = tournament::build_mixed_output(&specs, scores, rounds, &game);
    let mut json_val = serde_json::to_value(&output).unwrap_or(serde_json::json!({}));
    json_val["gpu"] = serde_json::json!(used_gpu);
    serde_json::to_string(&json_val).unwrap_or_else(|_| "{}".to_string())
}

/// CPU fallback for CA tournament using Rayon.
fn run_ca_tournament_cpu(
    rule_ids: &[u64],
    k: u8,
    two_r: u32,
    t: u32,
    rounds: u32,
    payoff: &tournament::DynPayoff,
    num_actions: u8,
) -> Vec<Vec<i64>> {
    let n = rule_ids.len();
    let r_f = two_r as f32 / 2.0;
    let specs: Vec<strategy::StrategySpec> = rule_ids
        .iter()
        .map(|&rule| strategy::StrategySpec::Ca { rule, k, r: r_f, t, num_actions })
        .collect();

    // Play all ordered pairs in parallel
    let pairs: Vec<(usize, usize)> = (0..n)
        .flat_map(|i| (0..n).filter(move |&j| i != j).map(move |j| (i, j)))
        .collect();

    let pair_scores: Vec<(usize, usize, i64)> = pairs
        .into_par_iter()
        .map(|(i, j)| {
            let mut runner_a = strategy::StrategyRunner::new(&specs[i]);
            let mut runner_b = strategy::StrategyRunner::new(&specs[j]);
            let (result, _) = strategy::play_game_dyn(&mut runner_a, &mut runner_b, rounds, payoff);
            let score_a = result.map(|(sa, _)| sa).unwrap_or(0);
            (i, j, score_a)
        })
        .collect();

    let mut matrix = vec![vec![0i64; n]; n];
    for (i, j, score) in pair_scores {
        matrix[i][j] = score;
    }
    matrix
}

/// Run a RuleArray tournament (CPU only).
/// Each rule array is a sequence of CA rule numbers applied step-by-step (inhomogeneous CA).
/// `rule_arrays_json` is a JSON array of arrays: [[r0,r1,...], [r0,r1,...], ...]
#[wll::export]
pub fn rule_array_tournament_wl(
    k: i64,
    r_numer: i64,
    r_denom: i64,
    t: i64,
    rounds: i64,
    game: String,
    rule_arrays_json: String,
    _use_gpu: bool,
) -> String {
    let k = k as u8;
    let r_f = r_numer as f32 / r_denom as f32;
    let t = t as u32;
    let rounds = rounds as u32;

    let dyn_payoff = match tournament::parse_game_dyn(&game) {
        Ok(p) => p,
        Err(e) => return format!("{{\"error\":\"{}\"}}", e),
    };
    let num_actions = dyn_payoff.num_actions as u8;

    let rule_arrays: Vec<Vec<u8>> = match serde_json::from_str(&rule_arrays_json) {
        Ok(v) => v,
        Err(e) => return format!("{{\"error\":\"{}\"}}", e),
    };
    if rule_arrays.is_empty() {
        return "{\"error\":\"no rule arrays provided\"}".to_string();
    }

    let specs: Vec<strategy::StrategySpec> = rule_arrays
        .iter()
        .map(|rules| strategy::StrategySpec::RuleArray {
            rules: rules.clone(),
            k,
            r: r_f,
            t,
            num_actions,
        })
        .collect();

    let n = specs.len();
    let pairs: Vec<(usize, usize)> = (0..n)
        .flat_map(|i| (0..n).filter(move |&j| i != j).map(move |j| (i, j)))
        .collect();

    let pair_scores: Vec<(usize, usize, i64)> = pairs
        .into_par_iter()
        .map(|(i, j)| {
            let mut runner_a = strategy::StrategyRunner::new(&specs[i]);
            let mut runner_b = strategy::StrategyRunner::new(&specs[j]);
            let (result, _) = strategy::play_game_dyn(&mut runner_a, &mut runner_b, rounds, &dyn_payoff);
            let score_a = result.map(|(sa, _)| sa).unwrap_or(0);
            (i, j, score_a)
        })
        .collect();

    let mut scores = vec![vec![0i64; n]; n];
    for (i, j, score) in pair_scores {
        scores[i][j] = score;
    }

    let output = tournament::build_mixed_output(&specs, scores, rounds, &game);
    serde_json::to_string(&output).unwrap_or_else(|_| "{}".to_string())
}

/// Classify CA rules by behavioral equivalence. Returns JSON with groups.
/// sample=0 means exhaustive (all rules), sample>0 means random sample of that size.
/// Uses Metal GPU when available, falls back to CPU (Rayon).
#[wll::export]
pub fn ca_classify_wl(
    k: i64,
    r_numer: i64,
    r_denom: i64,
    t: i64,
    depth: i64,
    sample: i64,
) -> String {
    let k = k as u8;
    let r_f = r_numer as f32 / r_denom as f32;
    let two_r = (2.0 * r_f).round() as u32;
    let t = t as u32;
    let depth = depth as u32;

    let neighborhood_size = (two_r + 1) as u32;
    let num_neighborhoods = (k as u64).pow(neighborhood_size);
    let total_rules = match (k as u64).checked_pow(num_neighborhoods as u32) {
        Some(n) => n,
        None => return "{\"error\":\"rule space overflow\"}".to_string(),
    };

    // Build candidate list: exhaustive or random sample
    let candidates: Vec<u64> = if sample > 0 {
        use std::collections::HashSet;
        let mut seen = HashSet::new();
        let target = (sample as u64).min(total_rules);
        let mut rng_state = 0x5DEECE66Du64.wrapping_mul(42).wrapping_add(0xBu64);
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
            let idx = full % total_rules;
            seen.insert(idx);
        }
        let mut v: Vec<u64> = seen.into_iter().collect();
        v.sort();
        v
    } else {
        (0..total_rules).collect()
    };

    let num_candidates = candidates.len() as u64;

    // Try Metal GPU for exhaustive enumeration (contiguous 0..total_rules)
    if sample == 0 && total_rules <= u32::MAX as u64 {
        match gpu::classify_ca_gpu(total_rules as u32, k, two_r, t, depth) {
            Ok((flat_sigs, sig_len)) => {
                return ca_classify_group_signatures_indexed(
                    &flat_sigs, sig_len, &candidates, total_rules,
                );
            }
            Err(_e) => {
            }
        }
    }

    // CPU path: build signatures with Rayon
    let sig_len = gpu::compute_ca_sig_len(depth) as usize;
    let mut flat_sigs = vec![0u8; num_candidates as usize * sig_len];

    flat_sigs
        .par_chunks_mut(sig_len)
        .enumerate()
        .for_each(|(i, chunk)| {
            let rule_code = candidates[i];
            let rule_table = strategy::decode_ca_rule_table(rule_code, k, two_r);
            let mut sig_idx = 0usize;
            for history_len in 0..=(depth * 2) {
                let num_histories = if history_len == 0 { 1 } else { (2u64).pow(history_len.min(20)) };
                let limit = num_histories.min(256);
                for h in 0..limit {
                    let history_bits: Vec<u8> = (0..history_len)
                        .map(|bit| ((h >> bit) & 1) as u8)
                        .collect();
                    chunk[sig_idx] = strategy::ca_move(&rule_table, k, two_r, t, &history_bits, 2);
                    sig_idx += 1;
                }
            }
        });

    ca_classify_group_signatures_indexed(&flat_sigs, sig_len, &candidates, total_rules)
}

/// Group rules by their behavioral signatures and return JSON.
/// `candidates` maps index -> rule_code for the rules that were actually classified.
fn ca_classify_group_signatures_indexed(
    flat_sigs: &[u8],
    sig_len: usize,
    candidates: &[u64],
    total_rules: u64,
) -> String {
    use std::collections::HashMap;
    let mut groups: HashMap<&[u8], Vec<u64>> = HashMap::new();
    for (i, &rule_code) in candidates.iter().enumerate() {
        let start = i * sig_len;
        let sig = &flat_sigs[start..start + sig_len];
        groups.entry(sig).or_default().push(rule_code);
    }

    let representatives: Vec<u64> = groups.values().map(|v| v[0]).collect();
    let group_entries: Vec<serde_json::Value> = groups.iter().map(|(_, members)| {
        serde_json::json!({
            "representative": members[0],
            "members": members,
            "size": members.len()
        })
    }).collect();

    let sampled = candidates.len() as u64;
    let output = serde_json::json!({
        "representatives": representatives,
        "groups": group_entries,
        "unique_behaviors": groups.len(),
        "total_rules": total_rules,
        "sampled_rules": sampled,
        "reduction_factor": if groups.len() > 0 { sampled as f64 / groups.len() as f64 } else { 0.0 }
    });

    serde_json::to_string(&output).unwrap_or_else(|_| "{}".to_string())
}

/// Classify FSMs by behavioral equivalence using two-step canonicalization.
/// Returns JSON with groups.
///
/// Two-step process (matching nit-games `fsm_enum.rs`):
/// 1. Canonicalize: decode -> BFS-reorder states -> structural key -> dedup
/// 2. Behavioral grouping: run canonical reps through all input sequences (12 steps)
///    -> group by output trace -> unique behavior representatives
///
/// `states`/`symbols` define the FSM space; `depth` is unused (kept for API compat,
/// trace length is fixed at 12 like nit); `sample=0` means exhaustive, `sample>0`
/// means random sample of that size.
#[wll::export]
pub fn fsm_classify_wl(
    states: i64,
    symbols: i64,
    _depth: i64,
    sample: i64,
) -> String {
    let states = states as usize;
    let symbols = symbols as usize;
    let sample = if sample > 0 { sample as usize } else { 0 };

    match strategy::fsm_classify_two_step(states, symbols, sample) {
        Ok(result) => {
            let group_entries: Vec<serde_json::Value> = result
                .groups
                .iter()
                .map(|members| {
                    serde_json::json!({
                        "representative": members[0],
                        "members": members,
                        "size": members.len()
                    })
                })
                .collect();

            let sampled = result.sampled_rules as u64;
            let output = serde_json::json!({
                "representatives": result.representatives,
                "groups": group_entries,
                "unique_behaviors": result.unique_behaviors,
                "total_rules": result.total_rules as u64,
                "canonical_count": result.canonical_count,
                "sampled_rules": sampled,
                "reduction_factor": if result.unique_behaviors > 0 {
                    sampled as f64 / result.unique_behaviors as f64
                } else {
                    0.0
                }
            });

            serde_json::to_string(&output).unwrap_or_else(|_| "{}".to_string())
        }
        Err(e) => format!("{{\"error\":\"{}\"}}", e),
    }
}

/// Run a GPU-accelerated FSM tournament. Falls back to CPU if Metal unavailable.
#[wll::export]
pub fn fsm_tournament_wl(
    states: i64,
    symbols: i64,
    rounds: i64,
    game: String,
    fsm_ids_json: String,
    use_gpu: bool,
) -> String {
    let states = states as usize;
    let symbols = symbols as usize;
    let rounds = rounds as u32;

    let dyn_payoff = match tournament::parse_game_dyn(&game) {
        Ok(p) => p,
        Err(e) => return format!("{{\"error\":\"{}\"}}", e),
    };
    let num_actions = dyn_payoff.num_actions as u8;

    let fsm_ids: Vec<u64> = match serde_json::from_str(&fsm_ids_json) {
        Ok(v) => v,
        Err(e) => return format!("{{\"error\":\"{}\"}}", e),
    };
    if fsm_ids.is_empty() {
        return "{\"error\":\"no FSM IDs provided\"}".to_string();
    }

    let scores: Vec<Vec<i64>>;
    let used_gpu: bool;

    #[cfg(all(target_os = "macos", feature = "metal"))]
    {
        if use_gpu {
            match gpu::run_fsm_tournament_gpu(
                &fsm_ids, states, symbols, rounds,
                dyn_payoff.num_actions as u32, &dyn_payoff.entries,
            ) {
                Ok(s) => {
                    scores = s;
                    used_gpu = true;
                }
                Err(_e) => {
                    scores = run_fsm_tournament_cpu(
                        &fsm_ids, states, symbols, rounds, &dyn_payoff, num_actions,
                    );
                    used_gpu = false;
                }
            };
        } else {
            scores = run_fsm_tournament_cpu(
                &fsm_ids, states, symbols, rounds, &dyn_payoff, num_actions,
            );
            used_gpu = false;
        }
    }
    #[cfg(not(all(target_os = "macos", feature = "metal")))]
    {
        let _ = use_gpu;
        scores = run_fsm_tournament_cpu(
            &fsm_ids, states, symbols, rounds, &dyn_payoff, num_actions,
        );
        used_gpu = false;
    }

    // Build output using mixed-output format with FSM specs
    let specs: Vec<strategy::StrategySpec> = fsm_ids
        .iter()
        .map(|&id| strategy::StrategySpec::Fsm {
            id,
            s: states as u16,
            k: symbols as u8,
            num_actions,
        })
        .collect();
    let output = tournament::build_mixed_output(&specs, scores, rounds, &game);
    let mut json_val = serde_json::to_value(&output).unwrap_or(serde_json::json!({}));
    json_val["gpu"] = serde_json::json!(used_gpu);
    serde_json::to_string(&json_val).unwrap_or_else(|_| "{}".to_string())
}

/// CPU fallback for FSM tournament using Rayon.
fn run_fsm_tournament_cpu(
    fsm_ids: &[u64],
    states: usize,
    symbols: usize,
    rounds: u32,
    payoff: &tournament::DynPayoff,
    num_actions: u8,
) -> Vec<Vec<i64>> {
    let n = fsm_ids.len();
    let specs: Vec<strategy::StrategySpec> = fsm_ids
        .iter()
        .map(|&id| strategy::StrategySpec::Fsm {
            id,
            s: states as u16,
            k: symbols as u8,
            num_actions,
        })
        .collect();

    // Play all ordered pairs in parallel
    let pairs: Vec<(usize, usize)> = (0..n)
        .flat_map(|i| (0..n).filter(move |&j| i != j).map(move |j| (i, j)))
        .collect();

    let pair_scores: Vec<(usize, usize, i64)> = pairs
        .into_par_iter()
        .map(|(i, j)| {
            let mut runner_a = strategy::StrategyRunner::new(&specs[i]);
            let mut runner_b = strategy::StrategyRunner::new(&specs[j]);
            let (result, _) = strategy::play_game_dyn(&mut runner_a, &mut runner_b, rounds, payoff);
            let score_a = result.map(|(sa, _)| sa).unwrap_or(0);
            (i, j, score_a)
        })
        .collect();

    let mut matrix = vec![vec![0i64; n]; n];
    for (i, j, score) in pair_scores {
        matrix[i][j] = score;
    }
    matrix
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
        assert!(count >= 1400 && count <= 1900,
                "TM(2,2) halting count {} outside [1400,1900]", count);
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

    // ── CA classify ─────────────────────────────────────────────────────

    #[test]
    fn ca_classify_k2_r_half_gpu_matches_cpu() {
        // k=2, r=1/2: 16 rules. Compare GPU vs CPU signatures.
        let k: u8 = 2;
        let two_r: u32 = 1;
        let t: u32 = 10;
        let depth: u32 = 4;
        let total_rules: u32 = 16;
        let sig_len = gpu::compute_ca_sig_len(depth) as usize;

        // CPU signatures
        let mut cpu_sigs = vec![0u8; total_rules as usize * sig_len];
        for rule_code in 0..total_rules as u64 {
            let rule_table = strategy::decode_ca_rule_table(rule_code, k, two_r);
            let base = rule_code as usize * sig_len;
            let mut idx = 0;
            for history_len in 0..=(depth * 2) {
                let num = if history_len == 0 { 1 } else { (2u64).pow(history_len.min(20)) };
                let limit = num.min(256);
                for h in 0..limit {
                    let bits: Vec<u8> = (0..history_len)
                        .map(|bit| ((h >> bit) & 1) as u8)
                        .collect();
                    cpu_sigs[base + idx] = strategy::ca_move(&rule_table, k, two_r, t, &bits, 2);
                    idx += 1;
                }
            }
        }

        // GPU signatures
        match gpu::classify_ca_gpu(total_rules, k, two_r, t, depth) {
            Ok((gpu_sigs, gpu_sig_len)) => {
                assert_eq!(gpu_sig_len, sig_len);
                assert_eq!(gpu_sigs.len(), cpu_sigs.len());
                for rule in 0..total_rules as usize {
                    let start = rule * sig_len;
                    let cpu_slice = &cpu_sigs[start..start + sig_len];
                    let gpu_slice = &gpu_sigs[start..start + sig_len];
                    assert_eq!(
                        cpu_slice, gpu_slice,
                        "GPU/CPU mismatch for rule {}", rule
                    );
                }
                eprintln!("  GPU vs CPU: all {} rules match (sig_len={})", total_rules, sig_len);
            }
            Err(e) => {
                eprintln!("  GPU not available, skipping comparison: {}", e);
            }
        }
    }

    // ── CA classify verification ────────────────────────────────────────

    #[test]
    fn ca_classify_k2_all_unique() {
        // For k=2, every rule is unique because table entries are already
        // in {0,1} and %2 is identity.
        // k=2, r=1/2 (numer=1, denom=2): 16 rules
        let result_str = ca_classify_wl(2, 1, 2, 10, 4, 0);
        let parsed: serde_json::Value = serde_json::from_str(&result_str).unwrap();
        let unique = parsed["unique_behaviors"].as_u64().unwrap();
        let total = parsed["total_rules"].as_u64().unwrap();
        assert_eq!(total, 16);
        assert_eq!(unique, 16, "k=2 r=1/2: all 16 rules should be unique");

        // k=2, r=1 (numer=1, denom=1): 256 rules
        let result_str = ca_classify_wl(2, 1, 1, 10, 4, 0);
        let parsed: serde_json::Value = serde_json::from_str(&result_str).unwrap();
        let unique = parsed["unique_behaviors"].as_u64().unwrap();
        let total = parsed["total_rules"].as_u64().unwrap();
        assert_eq!(total, 256);
        assert_eq!(unique, 256, "k=2 r=1: all 256 rules should be unique");
    }

    #[test]
    fn ca_classify_k3_r_half_has_equivalences() {
        // k=3, r=1/2: output %2 collapses 0 and 2, so there must be
        // fewer unique behaviors than total rules.
        // Total = 3^(3^2) = 3^9 = 19683
        let result_str = ca_classify_wl(3, 1, 2, 10, 4, 0);
        let parsed: serde_json::Value = serde_json::from_str(&result_str).unwrap();
        let total = parsed["total_rules"].as_u64().unwrap();
        let unique = parsed["unique_behaviors"].as_u64().unwrap();
        assert_eq!(total, 19683, "CA(3,1/2) should have 19683 total rules");
        assert!(
            unique < total,
            "k=3 should have equivalences: got {} unique out of {} total",
            unique, total
        );
        assert!(
            unique > 512,
            "unique count {} should exceed 512 (the number of distinct %2 tables for 9 entries)",
            unique
        );
        eprintln!("  CA(3,1/2): {} unique out of {} total (reduction {:.2}x)",
            unique, total, total as f64 / unique as f64);
    }

    #[test]
    fn ca_classify_k2_sample_all_unique() {
        // Sampling k=2 rules should still find all unique (no collisions)
        // since every k=2 rule is behaviorally distinct.
        // Use a small sample from a large space (k=2, r=3/2: 65536 rules)
        let result_str = ca_classify_wl(2, 3, 2, 10, 4, 1000);
        let parsed: serde_json::Value = serde_json::from_str(&result_str).unwrap();
        let sampled = parsed["sampled_rules"].as_u64().unwrap();
        let unique = parsed["unique_behaviors"].as_u64().unwrap();
        assert_eq!(sampled, 1000);
        assert_eq!(
            unique, sampled,
            "k=2 sampled: all {} samples should be unique, got {}",
            sampled, unique
        );
    }

    #[test]
    fn ca_classify_k4_r_half_has_equivalences() {
        // k=4, r=1/2: output %2 collapses {0,2}->0, {1,3}->1
        // Total = 4^(4^2) = 4^16 = 4294967296 (too big for exhaustive)
        // Sample and verify there ARE equivalences
        let result_str = ca_classify_wl(4, 1, 2, 10, 4, 10000);
        let parsed: serde_json::Value = serde_json::from_str(&result_str).unwrap();
        let sampled = parsed["sampled_rules"].as_u64().unwrap();
        let unique = parsed["unique_behaviors"].as_u64().unwrap();
        assert_eq!(sampled, 10000);
        assert!(
            unique < sampled,
            "k=4 should have equivalences in 10k samples: got {} unique",
            unique
        );
        eprintln!("  CA(4,1/2) sampled: {} unique out of {} (reduction {:.2}x)",
            unique, sampled, sampled as f64 / unique as f64);
    }

    #[test]
    fn ca_classify_reduction_factor_correct() {
        // Verify reduction_factor = sampled_rules / unique_behaviors
        let result_str = ca_classify_wl(3, 1, 2, 10, 4, 0);
        let parsed: serde_json::Value = serde_json::from_str(&result_str).unwrap();
        let sampled = parsed["sampled_rules"].as_f64().unwrap();
        let unique = parsed["unique_behaviors"].as_f64().unwrap();
        let reduction = parsed["reduction_factor"].as_f64().unwrap();
        let expected = sampled / unique;
        assert!(
            (reduction - expected).abs() < 0.001,
            "reduction_factor {} should equal sampled/unique = {}",
            reduction, expected
        );
    }

    #[test]
    fn ca_classify_group_sizes_sum_to_sampled() {
        // Every classified rule should appear in exactly one group
        let result_str = ca_classify_wl(3, 1, 2, 10, 4, 0);
        let parsed: serde_json::Value = serde_json::from_str(&result_str).unwrap();
        let sampled = parsed["sampled_rules"].as_u64().unwrap();
        let groups = parsed["groups"].as_array().unwrap();
        let total_in_groups: u64 = groups.iter()
            .map(|g| g["size"].as_u64().unwrap())
            .sum();
        assert_eq!(
            total_in_groups, sampled,
            "sum of group sizes {} should equal sampled rules {}",
            total_in_groups, sampled
        );
    }

    // ── CA tournament GPU vs CPU ────────────────────────────────────────

    #[test]
    fn ca_tournament_gpu_matches_cpu() {
        // Run a small CA(2, 1/2) tournament on GPU and CPU, compare scores.
        let rule_ids: Vec<u64> = (0..16).collect();
        let k: u8 = 2;
        let two_r: u32 = 1;
        let t: u32 = 10;
        let rounds: u32 = 50;
        let dyn_payoff = tournament::parse_game_dyn("pd").unwrap();
        let num_actions = dyn_payoff.num_actions as u8;

        let cpu_scores = run_ca_tournament_cpu(&rule_ids, k, two_r, t, rounds, &dyn_payoff, num_actions);

        match gpu::run_ca_tournament_gpu(&rule_ids, k, two_r, t, rounds, dyn_payoff.num_actions as u32, &dyn_payoff.entries) {
            Ok(gpu_scores) => {
                assert_eq!(gpu_scores.len(), cpu_scores.len());
                for i in 0..rule_ids.len() {
                    for j in 0..rule_ids.len() {
                        assert_eq!(
                            gpu_scores[i][j], cpu_scores[i][j],
                            "Score mismatch at [{},{}]: GPU={} CPU={}",
                            i, j, gpu_scores[i][j], cpu_scores[i][j]
                        );
                    }
                }
                eprintln!("  CA tournament GPU vs CPU: all scores match for {} CAs", rule_ids.len());
            }
            Err(e) => {
                eprintln!("  GPU not available, skipping CA tournament comparison: {}", e);
            }
        }
    }

    // ── FSM tournament GPU vs CPU ────────────────────────────────────

    #[test]
    fn fsm_tournament_gpu_matches_cpu() {
        let fsm_ids: Vec<u64> = vec![0, 1, 18, 19, 22, 23];
        let states = 2usize;
        let symbols = 2usize;
        let rounds = 50u32;
        let dyn_payoff = tournament::parse_game_dyn("pd").unwrap();
        let num_actions = dyn_payoff.num_actions as u8;

        let cpu_scores = run_fsm_tournament_cpu(
            &fsm_ids, states, symbols, rounds, &dyn_payoff, num_actions,
        );

        // Verify CPU scores are non-zero
        let cpu_nonzero = cpu_scores.iter()
            .flat_map(|row| row.iter())
            .any(|&s| s != 0);
        assert!(cpu_nonzero, "CPU FSM tournament should have non-zero scores");
        eprintln!("  CPU scores (first row): {:?}", &cpu_scores[0]);

        match gpu::run_fsm_tournament_gpu(
            &fsm_ids, states, symbols, rounds,
            dyn_payoff.num_actions as u32, &dyn_payoff.entries,
        ) {
            Ok(gpu_scores) => {
                eprintln!("  GPU scores (first row): {:?}", &gpu_scores[0]);
                let gpu_nonzero = gpu_scores.iter()
                    .flat_map(|row| row.iter())
                    .any(|&s| s != 0);
                assert!(gpu_nonzero, "GPU FSM tournament should have non-zero scores");

                assert_eq!(gpu_scores.len(), cpu_scores.len());
                for i in 0..fsm_ids.len() {
                    for j in 0..fsm_ids.len() {
                        assert_eq!(
                            gpu_scores[i][j], cpu_scores[i][j],
                            "Score mismatch at [{},{}]: GPU={} CPU={}",
                            i, j, gpu_scores[i][j], cpu_scores[i][j]
                        );
                    }
                }
                eprintln!("  FSM tournament GPU vs CPU: all scores match");
            }
            Err(e) => {
                eprintln!("  GPU not available, skipping FSM tournament comparison: {}", e);
            }
        }
    }

    #[test]
    fn metal_basic_write_test() {
        // Minimal test: compile and dispatch a trivial Metal kernel
        // that writes int2(42, -42) to a buffer. Verifies Metal works.
        #[cfg(all(target_os = "macos", feature = "metal"))]
        {
            use metal::*;
            use std::ffi::c_void;

            let device = Device::system_default().expect("no metal device");
            let source = r#"
                #include <metal_stdlib>
                using namespace metal;
                kernel void test_write(
                    device int2* out [[buffer(0)]],
                    uint gid [[thread_position_in_grid]])
                {
                    out[gid] = int2(42, -42);
                }
            "#;
            let options = CompileOptions::new();
            let library = device.new_library_with_source(source, &options)
                .expect("shader compile failed");
            let func = library.get_function("test_write", None)
                .expect("function not found");
            let pipeline = device.new_compute_pipeline_state_with_function(&func)
                .expect("pipeline failed");
            let queue = device.new_command_queue();

            let n = 10usize;
            let buf = device.new_buffer(
                (n * std::mem::size_of::<[i32; 2]>()) as u64,
                MTLResourceOptions::StorageModeShared,
            );
            unsafe {
                std::ptr::write_bytes(buf.contents() as *mut u8, 0, n * 8);
            }

            let cmd = queue.new_command_buffer();
            let enc = cmd.new_compute_command_encoder();
            enc.set_compute_pipeline_state(&pipeline);
            enc.set_buffer(0, Some(&buf), 0);
            let w = pipeline.thread_execution_width();
            enc.dispatch_thread_groups(
                MTLSize::new(((n as u64) + w - 1) / w, 1, 1),
                MTLSize::new(w, 1, 1),
            );
            enc.end_encoding();
            cmd.commit();
            cmd.wait_until_completed();

            let result = unsafe {
                let ptr = buf.contents() as *const [i32; 2];
                std::slice::from_raw_parts(ptr, n)
            };
            eprintln!("  Metal basic write: {:?}", &result[..3]);
            assert_eq!(result[0], [42, -42], "Metal kernel didn't write expected value");
        }
    }

    // ── FSM classify reduction table ────────────────────────────────────

    #[test]
    fn fsm_classify_reduction_table() {
        eprintln!();
        eprintln!("  {:<12} {:>10} {:>10} {:>10} {:>10} {:>10}",
            "Space", "Total", "Canonical", "Unique", "Struct.x", "Behav.x");
        eprintln!("  {}", "-".repeat(62));

        for (s, k) in [(1, 2), (2, 2), (3, 2), (4, 2)] {
            let result_str = fsm_classify_wl(s, k as i64, 4, 0);
            let parsed: serde_json::Value = serde_json::from_str(&result_str).unwrap();
            let total = parsed["total_rules"].as_u64().unwrap();
            let canonical = parsed["canonical_count"].as_u64().unwrap();
            let unique = parsed["unique_behaviors"].as_u64().unwrap();
            let structural_reduction = if canonical > 0 { total as f64 / canonical as f64 } else { 0.0 };
            let behavioral_reduction = if unique > 0 { canonical as f64 / unique as f64 } else { 0.0 };

            eprintln!("  FSM({},{})    {:>10} {:>10} {:>10} {:>10.1} {:>10.1}",
                s, k, total, canonical, unique, structural_reduction, behavioral_reduction);

            assert!(canonical <= total, "canonical should be <= total");
            assert!(unique <= canonical, "unique should be <= canonical");
            assert!(unique >= 1, "should have at least 1 unique behavior");
        }
        eprintln!();
    }
}
