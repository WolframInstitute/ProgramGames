//! FSM evolution (hill-climbing) entirely in Rust.
//!
//! Mirrors `EvolveResults.wl` / Code-02.nb's `MutateFSM`/`MutateFSMEdge`/
//! `MutateFSMStateAction` semantics, but runs the whole hill-climb loop
//! natively to avoid per-step WL <-> Rust FFI overhead.

use crate::strategy::{decode_fsm_raw, RawFsm};
use crate::tournament::DynPayoff;
use serde::Serialize;
use std::collections::VecDeque;

// ── Tiny seedable RNG (SplitMix64 for stream init, xoshiro256** for draws) ──

#[derive(Clone)]
pub struct Rng {
    s0: u64,
    s1: u64,
    s2: u64,
    s3: u64,
}

impl Rng {
    pub fn seed_from_u64(seed: u64) -> Self {
        // SplitMix64 to seed four lanes.
        let mut z = seed.wrapping_add(0x9E3779B97F4A7C15);
        let mut next = || {
            z = z.wrapping_add(0x9E3779B97F4A7C15);
            let mut x = z;
            x = (x ^ (x >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
            x = (x ^ (x >> 27)).wrapping_mul(0x94D049BB133111EB);
            x ^ (x >> 31)
        };
        let s0 = next();
        let s1 = next();
        let s2 = next();
        let s3 = next();
        // Avoid all-zero state.
        let mut r = Rng { s0, s1, s2, s3 };
        if (r.s0 | r.s1 | r.s2 | r.s3) == 0 {
            r.s0 = 1;
        }
        r
    }

    #[inline]
    pub fn next_u64(&mut self) -> u64 {
        // xoshiro256** core.
        let result = self.s1.wrapping_mul(5).rotate_left(7).wrapping_mul(9);
        let t = self.s1 << 17;
        self.s2 ^= self.s0;
        self.s3 ^= self.s1;
        self.s1 ^= self.s2;
        self.s0 ^= self.s3;
        self.s2 ^= t;
        self.s3 = self.s3.rotate_left(45);
        result
    }

    /// Uniform integer in [0, n). n must be > 0.
    #[inline]
    pub fn gen_range(&mut self, n: usize) -> usize {
        // Simple modulo (bias is negligible for the small ns we use here).
        if n == 0 {
            return 0;
        }
        (self.next_u64() as usize) % n
    }

    #[inline]
    pub fn coin(&mut self) -> bool {
        (self.next_u64() & 1) == 1
    }
}

// ── FSM helpers ─────────────────────────────────────────────────────────────

/// BFS reachable-state count from state 0.
pub fn reachable_count(fsm: &RawFsm) -> usize {
    let n = fsm.states();
    if n == 0 {
        return 0;
    }
    let mut seen = vec![false; n];
    seen[0] = true;
    let mut count = 1usize;
    let mut q = VecDeque::with_capacity(n);
    q.push_back(0usize);
    while let Some(s) = q.pop_front() {
        if let Some(row) = fsm.transitions.get(s) {
            for &nxt in row {
                if nxt < n && !seen[nxt] {
                    seen[nxt] = true;
                    count += 1;
                    q.push_back(nxt);
                }
            }
        }
    }
    count
}

/// Encode a RawFsm back to its (u128) index, matching Code-02.nb's
/// `FiniteStateMachineToIndex` byte-for-byte:
///   index = 1 + If[s==1, 0, FromDigits[nxt, s]] * k^s
///             + If[k==1, 0, FromDigits[out, k]]
/// where:
///   - `nxt` are the transition targets (0-indexed), MSB-first, in the
///      `Tuples[{Range[s], Range[0,k-1]}]` (state-major, input-minor) order.
///   - `out` is built by `out = ConstantArray[0, s];
///                       out[[targets]] = rule_outputs` — i.e. last-write-wins
///      keyed by transition target. States that are NOT reached by any
///      transition end up with output 0 in the encoded form, regardless of
///      what `fsm.outputs[state]` says. This matches WL's normalization and is
///      the *exact* inverse of `decode_fsm_raw` only for FSMs whose unreachable
///      states already carry output 0.
///
/// Edge cases: s == 0 or k == 0 → None. s == 1 forces tcode = 0. k == 1 forces
/// ocode = 0 (and `out` is irrelevant).
pub fn encode_fsm(fsm: &RawFsm) -> Option<u128> {
    let s = fsm.states() as u128;
    let k = fsm.actions as u128;
    if s == 0 || k == 0 {
        return None;
    }
    let states = fsm.states();
    let actions = fsm.actions;

    // transition_code: digits MSD-first, base s, length s*k.
    // Forced to 0 when s == 1 (matches WL's `If[s==1, 0, FromDigits[nxt, s]]`).
    let tcode: u128 = if states == 1 {
        0
    } else {
        let mut t: u128 = 0;
        for state_idx in 0..states {
            for input_idx in 0..actions {
                let d = fsm.transitions[state_idx][input_idx] as u128;
                t = t.checked_mul(s)?.checked_add(d)?;
            }
        }
        t
    };

    // output_code: WL builds `out = ConstantArray[0, s];
    //                         out[[targets+1]] = rule_outputs`
    // with last-write-wins. In RawFsm canonical form (Moore), every rule whose
    // transition targets state t carries output `outputs[t]`; thus the WL
    // assignment is equivalent to: out_enc[t] = outputs[t] iff some
    // transition lands on t, else out_enc[t] = 0.
    let ocode: u128 = if actions == 1 {
        0
    } else {
        let mut targeted = vec![false; states];
        for state_idx in 0..states {
            for input_idx in 0..actions {
                let t = fsm.transitions[state_idx][input_idx];
                if t < states {
                    targeted[t] = true;
                }
            }
        }
        let mut o: u128 = 0;
        for state_idx in 0..states {
            let d = if targeted[state_idx] {
                fsm.outputs[state_idx] as u128
            } else {
                0u128
            };
            o = o.checked_mul(k)?.checked_add(d)?;
        }
        o
    };

    let action_block = checked_pow_u128(k, states as u32)?;
    let prefix = tcode.checked_mul(action_block)?;
    let raw = prefix.checked_add(ocode)?;
    raw.checked_add(1)
}

fn checked_pow_u128(base: u128, exp: u32) -> Option<u128> {
    let mut value = 1u128;
    for _ in 0..exp {
        value = value.checked_mul(base)?;
    }
    Some(value)
}

// ── Mutation primitives ──────────────────────────────────────────────────────

/// MutateFSMStateAction: matches Code-02.nb exactly.
///
/// Code-02 picks one rule row {state, input} -> {next, out} uniformly from the
/// length-`s*k` rule list (ordered as `Tuples[{Range[s], Range[0,k-1]}]`,
/// i.e. state-major, input-minor), flips that row's output to a different
/// action in [0,k), then re-encodes via `FiniteStateMachineToIndex`. The
/// re-encoder walks all rules in order and assigns
/// `out[next_state] = this_row_output`, with last-write-wins semantics.
///
/// Because every rule pointing to the same next_state originally carries the
/// same output, the net effect on the Moore-canonical output table is:
///   - Let T = transitions[picked_state][picked_input].
///   - If the picked row is the LAST row in iteration order whose transition
///     targets T, then outputs[T] is flipped to newOut.
///   - Otherwise some later row writes the original output[T] back,
///     overwriting the flip — net effect is no change (a no-op mutation).
pub fn mutate_state_action(fsm: &RawFsm, rng: &mut Rng) -> RawFsm {
    let s = fsm.states();
    let k = fsm.actions;
    if k < 2 || s == 0 {
        return fsm.clone();
    }
    let n_rows = s * k;
    let idx = rng.gen_range(n_rows);
    let picked_state = idx / k;
    let picked_input = idx % k;
    let target = fsm.transitions[picked_state][picked_input];

    // Pick a new output ≠ outputs[target] (Code-02 uses RandomChoice on the
    // row's current output value, which equals outputs[target] in canonical
    // Moore encoding).
    let cur = fsm.outputs[target];
    let alt_count = k - 1;
    let pick = rng.gen_range(alt_count);
    let new_out = if pick < cur { pick } else { pick + 1 };

    // Determine the last row in state-major iteration order whose transition
    // targets `target`. If picked row != last writer, mutation is overwritten.
    let mut last_writer_idx: usize = usize::MAX;
    for st in 0..s {
        for inp in 0..k {
            if fsm.transitions[st][inp] == target {
                last_writer_idx = st * k + inp;
            }
        }
    }
    if idx != last_writer_idx {
        // Code-02's re-encoder would overwrite the flip — return unchanged.
        return fsm.clone();
    }
    let mut out = fsm.clone();
    out.outputs[target] = new_out;
    out
}

/// MutateFSMEdge: pick a "mutateable" (state, input) edge — one whose target
/// is shared by multiple incoming edges from distinct sources, plus self-loops.
/// Reroute its destination to a different state, keeping the edge's "output"
/// (i.e. the per-state outputs table) unchanged.
pub fn mutate_edge(fsm: &RawFsm, rng: &mut Rng) -> RawFsm {
    let s = fsm.states();
    let k = fsm.actions;
    if s < 2 {
        return fsm.clone();
    }

    // Compute, for each target state t, the set of distinct source states
    // that have an edge into t. Targets with |sources| > 1 are "mutateable".
    let mut sources_per_target: Vec<Vec<bool>> = vec![vec![false; s]; s];
    for src in 0..s {
        for inp in 0..k {
            let tgt = fsm.transitions[src][inp];
            if tgt < s {
                sources_per_target[tgt][src] = true;
            }
        }
    }
    let mutateable_target: Vec<bool> = sources_per_target
        .iter()
        .map(|row| row.iter().filter(|&&b| b).count() > 1)
        .collect();

    // Eligible edges: target ∈ mutateable, OR self-loop (src == tgt).
    let mut eligible: Vec<(usize, usize)> = Vec::new();
    for src in 0..s {
        for inp in 0..k {
            let tgt = fsm.transitions[src][inp];
            if mutateable_target.get(tgt).copied().unwrap_or(false) || src == tgt {
                eligible.push((src, inp));
            }
        }
    }
    if eligible.is_empty() {
        return fsm.clone();
    }
    let (src, inp) = eligible[rng.gen_range(eligible.len())];
    let cur = fsm.transitions[src][inp];
    // pick a different state
    let mut new_t = rng.gen_range(s);
    if new_t == cur {
        new_t = (cur + 1) % s;
    }
    let mut out = fsm.clone();
    out.transitions[src][inp] = new_t;
    out
}

/// MutateFSM with retry: coin-flip between MutateFSMStateAction and
/// MutateFSMEdge; require result ≠ original AND reachable_count(mutated) >=
/// reachable_count(original). Cap at 200 tries; return original if exhausted.
pub fn mutate_fsm(fsm: &RawFsm, rng: &mut Rng) -> RawFsm {
    let orig_reach = reachable_count(fsm);
    for _ in 0..200 {
        let cand = if rng.coin() {
            mutate_state_action(fsm, rng)
        } else {
            mutate_edge(fsm, rng)
        };
        // ≠ original?
        let differs = cand.outputs != fsm.outputs || cand.transitions != fsm.transitions;
        if !differs {
            continue;
        }
        if reachable_count(&cand) < orig_reach {
            continue;
        }
        return cand;
    }
    fsm.clone()
}

// ── Scoring kernel (FSM vs FSM) ─────────────────────────────────────────────

/// Mean payoff for player A over `rounds` rounds against opponent `b`.
/// Operates directly on RawFsms — no per-step decode, no StrategyRunner.
/// Round 0: emit each FSM's initial-state output. Round 1+: transition on
/// opponent's previous move, then emit output.
pub fn score_vs(a: &RawFsm, b: &RawFsm, rounds: u32, payoff: &DynPayoff) -> f64 {
    if rounds == 0 {
        return 0.0;
    }
    let na = payoff.num_actions;
    let mut state_a = 0usize;
    let mut state_b = 0usize;
    let mut prev_a: u8 = 0;
    let mut prev_b: u8 = 0;
    let mut total: i64 = 0;

    for round in 0..rounds {
        let (move_a, move_b) = if round == 0 {
            (
                (a.outputs[state_a] % a.actions.max(1)) as u8,
                (b.outputs[state_b] % b.actions.max(1)) as u8,
            )
        } else {
            // Transition on opponent's last action (clipped to k-1).
            let inp_a = (prev_b as usize).min(a.actions - 1);
            let inp_b = (prev_a as usize).min(b.actions - 1);
            state_a = a.transitions[state_a][inp_a];
            state_b = b.transitions[state_b][inp_b];
            (
                (a.outputs[state_a] % a.actions.max(1)) as u8,
                (b.outputs[state_b] % b.actions.max(1)) as u8,
            )
        };
        let ma = (move_a as usize).min(na - 1);
        let mb = (move_b as usize).min(na - 1);
        if let Some(p) = payoff.get(ma * na + mb) {
            total += p[0] as i64;
        }
        prev_a = move_a;
        prev_b = move_b;
    }

    total as f64 / rounds as f64
}

/// Mean payoff for `a` against a list of opponents.
pub fn score_vs_many(a: &RawFsm, opps: &[RawFsm], rounds: u32, payoff: &DynPayoff) -> f64 {
    if opps.is_empty() {
        return 0.0;
    }
    let mut sum = 0.0f64;
    for o in opps {
        sum += score_vs(a, o, rounds, payoff);
    }
    sum / opps.len() as f64
}

// ── Trajectory entry types ──────────────────────────────────────────────────

#[derive(Serialize)]
pub struct StepEntry {
    /// FSM index as decimal string (may exceed u64 for s=9,k=2).
    pub id: String,
    pub s: u64,
    pub k: u64,
    pub fitness: f64,
    pub accepted: bool,
    pub candidate_fitness: f64,
}

#[derive(Serialize)]
pub struct CoadaptEntry {
    pub id_a: String,
    pub s_a: u64,
    pub k_a: u64,
    pub id_b: String,
    pub s_b: u64,
    pub k_b: u64,
    pub score_a: f64,
    pub score_b: f64,
}

fn fsm_to_id_string(fsm: &RawFsm) -> String {
    encode_fsm(fsm).map(|i| i.to_string()).unwrap_or_else(|| "0".to_string())
}

// ── Evolve loops ────────────────────────────────────────────────────────────

pub fn evolve_single(
    start: &RawFsm,
    opp: &RawFsm,
    rounds: u32,
    steps: usize,
    payoff: &DynPayoff,
    seed: u64,
) -> Vec<StepEntry> {
    let mut rng = Rng::seed_from_u64(seed);
    let mut cur = start.clone();
    let mut cur_fit = score_vs(&cur, opp, rounds, payoff);
    let mut traj = Vec::with_capacity(steps + 1);
    traj.push(StepEntry {
        id: fsm_to_id_string(&cur),
        s: cur.states() as u64,
        k: cur.actions as u64,
        fitness: cur_fit,
        accepted: true,
        candidate_fitness: cur_fit,
    });
    for _ in 0..steps {
        let cand = mutate_fsm(&cur, &mut rng);
        let cand_fit = score_vs(&cand, opp, rounds, payoff);
        let accept = cand_fit >= cur_fit;
        if accept {
            cur = cand;
            cur_fit = cand_fit;
        }
        traj.push(StepEntry {
            id: fsm_to_id_string(&cur),
            s: cur.states() as u64,
            k: cur.actions as u64,
            fitness: cur_fit,
            accepted: accept,
            candidate_fitness: cand_fit,
        });
    }
    traj
}

pub fn evolve_population(
    start: &RawFsm,
    opps: &[RawFsm],
    rounds: u32,
    steps: usize,
    payoff: &DynPayoff,
    seed: u64,
) -> Vec<StepEntry> {
    let mut rng = Rng::seed_from_u64(seed);
    let mut cur = start.clone();
    let mut cur_fit = score_vs_many(&cur, opps, rounds, payoff);
    let mut traj = Vec::with_capacity(steps + 1);
    traj.push(StepEntry {
        id: fsm_to_id_string(&cur),
        s: cur.states() as u64,
        k: cur.actions as u64,
        fitness: cur_fit,
        accepted: true,
        candidate_fitness: cur_fit,
    });
    for _ in 0..steps {
        let cand = mutate_fsm(&cur, &mut rng);
        let cand_fit = score_vs_many(&cand, opps, rounds, payoff);
        let accept = cand_fit >= cur_fit;
        if accept {
            cur = cand;
            cur_fit = cand_fit;
        }
        traj.push(StepEntry {
            id: fsm_to_id_string(&cur),
            s: cur.states() as u64,
            k: cur.actions as u64,
            fitness: cur_fit,
            accepted: accept,
            candidate_fitness: cand_fit,
        });
    }
    traj
}

pub fn evolve_coadapt(
    start_a: &RawFsm,
    start_b: &RawFsm,
    rounds: u32,
    steps: usize,
    payoff: &DynPayoff,
    payoff_swapped: &DynPayoff,
    seed: u64,
) -> Vec<CoadaptEntry> {
    let mut rng = Rng::seed_from_u64(seed);
    let mut a = start_a.clone();
    let mut b = start_b.clone();
    // p1 = mean payoff of A; p2 = mean payoff of B (= score with swapped roles)
    let mut p1 = score_vs(&a, &b, rounds, payoff);
    let mut p2 = score_vs(&b, &a, rounds, payoff_swapped);

    let mut hist = Vec::with_capacity(steps + 1);
    hist.push(CoadaptEntry {
        id_a: fsm_to_id_string(&a),
        s_a: a.states() as u64,
        k_a: a.actions as u64,
        id_b: fsm_to_id_string(&b),
        s_b: b.states() as u64,
        k_b: b.actions as u64,
        score_a: p1,
        score_b: p2,
    });

    for step in 1..=steps {
        if step % 2 == 1 {
            // mutate A
            let cand = mutate_fsm(&a, &mut rng);
            let sa = score_vs(&cand, &b, rounds, payoff);
            let sb = score_vs(&b, &cand, rounds, payoff_swapped);
            if sa >= p1 {
                a = cand;
                p1 = sa;
                p2 = sb;
            }
        } else {
            // mutate B
            let cand = mutate_fsm(&b, &mut rng);
            let sa = score_vs(&a, &cand, rounds, payoff);
            let sb = score_vs(&cand, &a, rounds, payoff_swapped);
            if sb >= p2 {
                b = cand;
                p1 = sa;
                p2 = sb;
            }
        }
        hist.push(CoadaptEntry {
            id_a: fsm_to_id_string(&a),
            s_a: a.states() as u64,
            k_a: a.actions as u64,
            id_b: fsm_to_id_string(&b),
            s_b: b.states() as u64,
            k_b: b.actions as u64,
            score_a: p1,
            score_b: p2,
        });
    }
    hist
}

// ── Single-step and ensemble drivers ────────────────────────────────────────

/// One mutation step against a list of opponents (mean payoff if many).
/// Returns (next_fsm, next_fit, accepted, candidate_fit).
pub fn evolve_step(
    cur: &RawFsm,
    cur_fit: f64,
    opps: &[RawFsm],
    rounds: u32,
    payoff: &DynPayoff,
    rng: &mut Rng,
) -> (RawFsm, f64, bool, f64) {
    let cand = mutate_fsm(cur, rng);
    let cand_fit = if opps.len() == 1 {
        score_vs(&cand, &opps[0], rounds, payoff)
    } else {
        score_vs_many(&cand, opps, rounds, payoff)
    };
    if cand_fit >= cur_fit {
        (cand, cand_fit, true, cand_fit)
    } else {
        (cur.clone(), cur_fit, false, cand_fit)
    }
}

/// Generic n-step trajectory against one or many opponents.
pub fn evolve_trajectory(
    start: &RawFsm,
    opps: &[RawFsm],
    rounds: u32,
    steps: usize,
    payoff: &DynPayoff,
    seed: u64,
) -> Vec<StepEntry> {
    if opps.len() == 1 {
        evolve_single(start, &opps[0], rounds, steps, payoff, seed)
    } else {
        evolve_population(start, opps, rounds, steps, payoff, seed)
    }
}

/// Run N independent hill-climbs from a list of starting FSMs in parallel.
/// Each run uses seed = base_seed + i.
pub fn evolve_ensemble(
    starts: &[RawFsm],
    opps: &[RawFsm],
    rounds: u32,
    steps: usize,
    payoff: &DynPayoff,
    base_seed: u64,
) -> Vec<Vec<StepEntry>> {
    use rayon::prelude::*;
    starts
        .par_iter()
        .enumerate()
        .map(|(i, s)| {
            evolve_trajectory(s, opps, rounds, steps, payoff, base_seed.wrapping_add(i as u64))
        })
        .collect()
}

// ── Top-level convenience: decode index strings -> RawFsm ───────────────────

pub fn decode_id_str(id_str: &str, s: usize, k: usize) -> Result<RawFsm, String> {
    // Re-use decode_fsm_raw via u64 fast path; fall back to bigint via
    // the existing decode_fsm_from_str (which we adapt by going via decode_fsm).
    if let Ok(idx) = id_str.trim().parse::<u64>() {
        return decode_fsm_raw(idx, s, k);
    }
    // For huge ids (> u64), use the bigint string path for `decode_fsm_from_str`
    // and then re-shape into RawFsm.
    let (outputs_flat, transitions_flat) =
        crate::strategy::decode_fsm_from_str(id_str, s, k);
    let outputs: Vec<usize> = outputs_flat.into_iter().map(|x| x as usize).collect();
    let mut transitions = vec![vec![0usize; k]; s];
    for state_idx in 0..s {
        for input_idx in 0..k {
            transitions[state_idx][input_idx] = transitions_flat[state_idx * k + input_idx];
        }
    }
    Ok(RawFsm { outputs, transitions, actions: k })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tournament::parse_game_dyn;

    #[test]
    fn encode_decode_roundtrip() {
        // For canonical indices (those that survive `encode∘decode` once),
        // a second round-trip must be identity. This is the WL contract:
        // FiniteStateMachineToIndex[FiniteStateMachineToRule[{i,s,k}]]
        // is idempotent after one application.
        for idx in [0u64, 1, 17, 1164, 5000, 5831] {
            let raw = decode_fsm_raw(idx, 3, 2).unwrap();
            let canonical = encode_fsm(&raw).unwrap() as u64;
            let raw2 = decode_fsm_raw(canonical, 3, 2).unwrap();
            let back = encode_fsm(&raw2).unwrap();
            assert_eq!(
                back, canonical as u128,
                "fixed-point failed for idx={idx} canonical={canonical}"
            );
        }
    }

    #[test]
    fn encode_decode_roundtrip_random() {
        // For s=2..5, k=2, verify encode∘decode is idempotent (a fixed point
        // after one application) for 100 random indices each.
        for s in 2usize..=5 {
            let max = crate::strategy::fsm_count(s, 2).expect("count fits") as u64;
            let mut state: u64 = 0x9E37_79B9_7F4A_7C15u64.wrapping_add(s as u64);
            for _ in 0..100 {
                state = state
                    .wrapping_mul(6364136223846793005)
                    .wrapping_add(1442695040888963407);
                let idx = state % max;
                let raw = decode_fsm_raw(idx, s, 2).unwrap();
                let canonical = encode_fsm(&raw).unwrap() as u64;
                let raw2 = decode_fsm_raw(canonical, s, 2).unwrap();
                let back = encode_fsm(&raw2).unwrap();
                assert_eq!(
                    back, canonical as u128,
                    "fixed-point failed for s={s} idx={idx}"
                );
            }
        }
    }

    #[test]
    fn decode_matches_wl_ground_truth() {
        // Hand-computed via WL's FiniteStateMachineToRule for s=3, k=2.
        //
        // For idx=0: QuotientRemainder[-1, 8] = (-1, 7).
        //   nxt = IntegerDigits[-1, 3, 6] = digits of |−1| = {0,0,0,0,0,1}
        //   out = IntegerDigits[7, 2, 3]  = {1,1,1}
        //   transitions (state-major): (s0,i0)->0, (s0,i1)->0, (s1,i0)->0,
        //   (s1,i1)->0, (s2,i0)->0, (s2,i1)->1.
        //   Outputs at unreachable state 2 → 0; states 0,1 reached → 1.
        let raw = decode_fsm_raw(0, 3, 2).unwrap();
        assert_eq!(raw.transitions, vec![vec![0, 0], vec![0, 0], vec![0, 1]]);
        // Decoder mirrors WL's IntegerDigits[7,2,3] = {1,1,1}; outputs at
        // unreached states are normalized away inside `encode_fsm` only.
        assert_eq!(raw.outputs, vec![1, 1, 1]);
        // idx=0 is non-canonical: state 2 is unreached, so its decoded
        // output isn't observable in the WL rule. The canonical encoding
        // for this rule is 15 (= 1 + 1*8 + 6, where ocode = digits {1,1,0}
        // after zeroing the unreached state's output).
        assert_eq!(encode_fsm(&raw).unwrap(), 15);

        // For idx=1: QuotientRemainder[0, 8] = (0, 0). nxt={0,...,0}, out={0,0,0}.
        // All transitions → state 0; outputs zeroed. This *is* canonical.
        let raw = decode_fsm_raw(1, 3, 2).unwrap();
        assert_eq!(raw.transitions, vec![vec![0, 0], vec![0, 0], vec![0, 0]]);
        assert_eq!(raw.outputs, vec![0, 0, 0]);
        assert_eq!(encode_fsm(&raw).unwrap(), 1);

        // Spot-check that encode∘decode is idempotent across [0, 30].
        for idx in 0..=30u64 {
            let raw = decode_fsm_raw(idx, 3, 2).unwrap();
            let canonical = encode_fsm(&raw).unwrap() as u64;
            let raw2 = decode_fsm_raw(canonical, 3, 2).unwrap();
            let back = encode_fsm(&raw2).unwrap();
            assert_eq!(back, canonical as u128, "fixed-point failed for idx={idx}");
        }
    }

    #[test]
    fn reachable_two_state() {
        let raw = decode_fsm_raw(1, 2, 2).unwrap();
        assert!(reachable_count(&raw) >= 1);
    }

    #[test]
    fn evolve_runs() {
        // Matching pennies: P1 wins on (0,0)/(1,1)
        let mp = parse_game_dyn("1,-1,-1,1,-1,1,1,-1").unwrap();
        let start = decode_fsm_raw(0, 3, 2).unwrap();
        let opp = decode_fsm_raw(1164, 3, 2).unwrap();
        let traj = evolve_single(&start, &opp, 200, 30, &mp, 42);
        assert_eq!(traj.len(), 31);
        // Monotone non-decreasing fitness curve.
        for w in traj.windows(2) {
            assert!(w[1].fitness >= w[0].fitness - 1e-9);
        }
    }
}
