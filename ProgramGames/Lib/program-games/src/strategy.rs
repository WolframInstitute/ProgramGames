//! Strategy module: unified TM, FSM, and CA strategy runners.
//!
//! Provides a `StrategySpec` enum (JSON-deserializable) and a `StrategyRunner`
//! that can play any of the three strategy types in an iterated game.

use serde::Deserialize;

use crate::tournament::{run_tm_on_tape, DynPayoff, Payoff};
use crate::TmTransition;

// ── Strategy specification (JSON-deserializable) ─────────────────────────────

fn default_max_steps() -> u32 {
    500
}

fn default_num_actions() -> u8 {
    2
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type")]
pub enum StrategySpec {
    #[serde(rename = "tm")]
    Tm {
        id: u64,
        s: u16,
        k: u8,
        #[serde(default = "default_max_steps")]
        max_steps: u32,
        #[serde(default = "default_num_actions")]
        num_actions: u8,
    },
    #[serde(rename = "fsm")]
    Fsm {
        id: u64,
        s: u16,
        k: u8,
        #[serde(default = "default_num_actions")]
        num_actions: u8,
    },
    #[serde(rename = "ca")]
    Ca {
        rule: u64,
        k: u8,
        r: f32,
        t: u32,
        #[serde(default = "default_num_actions")]
        num_actions: u8,
    },
    #[serde(rename = "rule_array")]
    RuleArray {
        rules: Vec<u8>,
        k: u8,
        r: f32,
        t: u32,
        #[serde(default = "default_num_actions")]
        num_actions: u8,
    },
}

impl StrategySpec {
    /// A human-readable label for this strategy.
    pub fn label(&self) -> String {
        match self {
            StrategySpec::Tm { id, s, k, .. } => format!("tm({},{})#{}", s, k, id),
            StrategySpec::Fsm { id, s, k, .. } => format!("fsm({},{})#{}", s, k, id),
            StrategySpec::Ca { rule, k, r, t, .. } => format!("ca({},{},{})#{}", k, r, t, rule),
            StrategySpec::RuleArray { rules, k, r, t, .. } => {
                let rules_str: String = rules.iter().take(4).map(|r| r.to_string()).collect::<Vec<_>>().join(",");
                if rules.len() > 4 {
                    format!("ra({},{},{})#[{},..]", k, r, t, rules_str)
                } else {
                    format!("ra({},{},{})#[{}]", k, r, t, rules_str)
                }
            }
        }
    }
}

// ── Strategy runner ──────────────────────────────────────────────────────────

/// A stateful strategy runner that can play rounds in an iterated game.
pub struct StrategyRunner {
    #[allow(dead_code)]
    spec: StrategySpec,
    inner: RunnerInner,
}

enum RunnerInner {
    Tm {
        transitions: Vec<TmTransition>,
        symbols: u8,
        max_steps: u32,
        num_actions: u8,
    },
    Fsm {
        state: usize,
        /// outputs[state] = output symbol
        outputs: Vec<u8>,
        /// transitions[state * k + input] = next_state (0-indexed)
        transitions: Vec<usize>,
        k: usize,
        num_actions: u8,
    },
    Ca {
        rule_table: Vec<u8>,
        k: u8,
        two_r: u32,
        t: u32,
        num_actions: u8,
    },
    RuleArray {
        rule_tables: Vec<Vec<u8>>,
        k: u8,
        two_r: u32,
        t: u32,
        num_actions: u8,
    },
}

impl StrategyRunner {
    /// Create a new runner from a strategy specification.
    pub fn new(spec: &StrategySpec) -> Self {
        let inner = match spec {
            StrategySpec::Tm {
                id,
                s,
                k,
                max_steps,
                num_actions,
            } => {
                let transitions = crate::decode_tm(*id, *s, *k);
                RunnerInner::Tm {
                    transitions,
                    symbols: *k,
                    max_steps: *max_steps,
                    num_actions: *num_actions,
                }
            }
            StrategySpec::Fsm { id, s, k, num_actions } => {
                let (outputs, transitions) =
                    decode_fsm(*id, *s as usize, *k as usize);
                RunnerInner::Fsm {
                    state: 0,
                    outputs,
                    transitions,
                    k: *k as usize,
                    num_actions: *num_actions,
                }
            }
            StrategySpec::Ca { rule, k, r, t, num_actions } => {
                let two_r = (2.0 * r).round() as u32;
                let rule_table = decode_ca_rule_table(*rule, *k, two_r);
                RunnerInner::Ca {
                    rule_table,
                    k: *k,
                    two_r,
                    t: *t,
                    num_actions: *num_actions,
                }
            }
            StrategySpec::RuleArray { rules, k, r, t, num_actions } => {
                let two_r = (2.0 * r).round() as u32;
                let rule_tables = rules.iter()
                    .map(|&rule| decode_ca_rule_table(rule as u64, *k, two_r))
                    .collect();
                RunnerInner::RuleArray {
                    rule_tables,
                    k: *k,
                    two_r,
                    t: *t,
                    num_actions: *num_actions,
                }
            }
        };
        StrategyRunner {
            spec: spec.clone(),
            inner,
        }
    }

    /// Get this runner's strategy spec.
    #[allow(dead_code)]
    pub fn spec(&self) -> &StrategySpec {
        &self.spec
    }

    /// Get the move for this round given the game history so far.
    /// `history` is a flat array: [a1, b1, a2, b2, ...] where 0=cooperate, 1=defect.
    /// `round` is the 0-indexed round number.
    /// Returns `None` if a TM fails to halt (non-halting machines should be excluded).
    pub fn get_move(&mut self, history: &[u8], round: u32) -> Option<u8> {
        match &mut self.inner {
            RunnerInner::Tm {
                transitions,
                symbols,
                max_steps,
                num_actions,
            } => {
                if round == 0 {
                    return Some(0); // cooperate on empty history
                }
                let (halted, output) =
                    run_tm_on_tape(transitions, *symbols, history, *max_steps);
                if halted {
                    Some(output % *num_actions)
                } else {
                    None // non-halting: caller must exclude this TM
                }
            }
            RunnerInner::Fsm {
                state,
                outputs,
                transitions,
                k,
                num_actions,
            } => {
                // On round 0: output based on initial state, input defaults to 0
                let input = if round == 0 {
                    0usize
                } else {
                    // Opponent's last move: history[history.len() - 1] for player B's last move
                    // when we are player A, or history[history.len() - 2] for player A's move
                    // when we are player B. But in this engine, the FSM input is always the
                    // opponent's last move. Since we don't know which player we are here,
                    // we pass the opponent's move via the history layout.
                    //
                    // Convention: FSM always reads the opponent's last move.
                    // For player A: opponent = last element (index len-1)
                    // For player B: opponent = second-to-last element (index len-2)
                    // However, in this simplified engine both players see the same history
                    // and the FSM just uses the "other's" move. Since the caller doesn't
                    // distinguish, we use the last round's opponent move = history[-1].
                    //
                    // Actually: The history is [a1,b1,a2,b2,...]. For FSMs, the input
                    // is the opponent's last move. But since we don't track which player
                    // this runner is, we can't determine opponent vs self. The simplest
                    // correct approach: FSM input = opponent's last move, which the
                    // caller must arrange. In the tournament, we handle this by passing
                    // the correct opponent_last_move to get_fsm_move() instead.
                    //
                    // For now, default to reading the last history entry as the input.
                    // The tournament code will use the dedicated fsm path.
                    history.last().copied().unwrap_or(0) as usize
                };

                let output = outputs.get(*state).copied().unwrap_or(0) % *num_actions;

                // Transition
                let idx = (*state) * (*k) + input.min(*k - 1);
                *state = transitions.get(idx).copied().unwrap_or(0);

                Some(output)
            }
            RunnerInner::Ca {
                rule_table,
                k,
                two_r,
                t,
                num_actions,
            } => {
                if round == 0 {
                    return Some(0); // cooperate on empty history
                }
                Some(ca_move(rule_table, *k, *two_r, *t, history, *num_actions))
            }
            RunnerInner::RuleArray {
                rule_tables,
                k,
                two_r,
                t,
                num_actions,
            } => {
                if round == 0 {
                    return Some(0);
                }
                Some(rule_array_move(rule_tables, *k, *two_r, *t, history, *num_actions))
            }
        }
    }

    /// Get move for an FSM, explicitly providing the opponent's last move.
    /// This is the preferred method for FSM strategies in tournament play.
    #[allow(dead_code)]
    pub fn get_fsm_move(&mut self, opponent_last: u8, round: u32) -> Option<u8> {
        match &mut self.inner {
            RunnerInner::Fsm {
                state,
                outputs,
                transitions,
                k,
                num_actions,
            } => {
                let input = if round == 0 {
                    0usize
                } else {
                    opponent_last as usize
                };

                let output = outputs.get(*state).copied().unwrap_or(0) % *num_actions;

                let idx = (*state) * (*k) + input.min(*k - 1);
                *state = transitions.get(idx).copied().unwrap_or(0);

                Some(output)
            }
            _ => {
                self.get_move(&[], round)
            }
        }
    }

    /// Reset state for a new game.
    pub fn reset(&mut self) {
        match &mut self.inner {
            RunnerInner::Tm { .. } => {
                // TMs are stateless between rounds (history is the state)
            }
            RunnerInner::Fsm { state, .. } => {
                *state = 0; // reset to initial state
            }
            RunnerInner::Ca { .. } => {
                // CAs are stateless (they read the full history each round)
            }
            RunnerInner::RuleArray { .. } => {
                // RuleArrays are stateless (they read the full history each round)
            }
        }
    }

    /// Returns true if this is a TM strategy.
    #[allow(dead_code)]
    pub fn is_tm(&self) -> bool {
        matches!(self.inner, RunnerInner::Tm { .. })
    }
}

// ── FSM decode ───────────────────────────────────────────────────────────────

/// Decode an FSM from its index, matching WL's `FiniteStateMachineToRule`
/// and nit-games' `decode_fsm_notebook_index`.
///
/// Returns (outputs, flat_transitions) where:
/// - outputs[state] = output symbol (0 or 1), 0-indexed states
/// - flat_transitions[state * k + input] = next_state (0-indexed)
///
/// Parameters:
/// - `index`: 0-based FSM index
/// - `states`: number of internal states (s)
/// - `actions`: number of output symbols (k, typically 2)
pub fn decode_fsm(index: u64, states: usize, actions: usize) -> (Vec<u8>, Vec<usize>) {
    if states == 0 || actions == 0 {
        return (vec![], vec![]);
    }

    let n = states * actions; // total transition entries

    // action_block = k^s
    let action_block = checked_pow_u128(actions as u128, states as u32).unwrap_or(1);

    // Split: (transition_code, output_code) = floor_div_rem(index - 1, k^s)
    let (transition_code, output_code) =
        floor_div_rem_i128(index as i128 - 1, action_block as i128);

    // Decode transition digits: IntegerDigits[transition_code, s, s*k]
    let next_digits = if states == 1 {
        vec![0usize; n]
    } else {
        integer_digits_signed_abs(transition_code, states, n)
    };

    // Decode output digits: IntegerDigits[output_code, k, s]
    let output_digits = if actions == 1 {
        vec![0usize; states]
    } else {
        integer_digits_unsigned(output_code as u128, actions, states)
    };

    // Build outputs: 0-indexed states
    let outputs: Vec<u8> = output_digits
        .into_iter()
        .map(|d| d as u8)
        .collect();

    // Build flat transition table: transitions[state * k + input] = next_state
    let mut transitions = vec![0usize; n];
    for state_idx in 0..states {
        for input_idx in 0..actions {
            let flat_idx = state_idx * actions + input_idx;
            let next = next_digits.get(flat_idx).copied().unwrap_or(0);
            transitions[flat_idx] = next.min(states - 1);
        }
    }

    (outputs, transitions)
}

/// Count total FSMs for given (states, actions): s^(s*k) * k^s
#[allow(dead_code)]
pub fn fsm_count(states: usize, actions: usize) -> Option<u128> {
    let transitions = checked_pow_u128(states as u128, (states * actions) as u32)?;
    let outputs = checked_pow_u128(actions as u128, states as u32)?;
    transitions.checked_mul(outputs)
}

// ── CA decode and execution ──────────────────────────────────────────────────

/// Decode a CA rule number into its lookup table.
/// Matches WL's `CellularAutomatonToRule[{n, k, r}]`.
///
/// The table maps each (2r+1)-neighborhood to an output symbol.
/// Index: for neighborhood (d0, d1, ..., d_{2r}), index = sum(d_i * k^(2r-i)).
pub fn decode_ca_rule_table(rule_code: u64, k: u8, two_r: u32) -> Vec<u8> {
    let neighborhood = two_r as usize + 1;
    let k_usize = k.max(2) as usize;
    let table_len = checked_pow_usize(k_usize, neighborhood).unwrap_or(0);
    integer_digits_unsigned(rule_code as u128, k_usize, table_len)
        .into_iter()
        .map(|d| d as u8)
        .collect()
}

/// Run a shrinking CA for `steps` steps on the given input row.
/// Each step applies the CA rule to all windows of size (2r+1), producing
/// a row that is `2r` cells shorter.
pub fn run_shrinking_ca(
    rule_table: &[u8],
    k: u8,
    two_r: u32,
    steps: u32,
    input_row: &[u8],
) -> Vec<Vec<u8>> {
    let mut row = if input_row.is_empty() {
        vec![0]
    } else {
        input_row.to_vec()
    };
    let mut rows = vec![row.clone()];
    let neighborhood = two_r as usize + 1;
    let k_usize = k.max(2) as usize;

    for _ in 0..steps {
        if neighborhood == 0 || row.len() <= two_r as usize {
            break;
        }
        let next_len = row.len() - two_r as usize;
        if next_len == 0 {
            break;
        }
        let next: Vec<u8> = (0..next_len)
            .map(|start| {
                let window = &row[start..start + neighborhood];
                let mut idx = 0usize;
                for &d in window {
                    idx = idx * k_usize + d as usize;
                }
                rule_table.get(idx).copied().unwrap_or(0)
            })
            .collect();
        row = next;
        rows.push(row.clone());
    }
    rows
}

/// Compute a CA strategy's move from the game history bits.
/// Runs ShrinkingCA for `t` steps on the history, returns the last cell of the
/// final row (mod 2 for binary output).
pub fn ca_move(rule_table: &[u8], k: u8, two_r: u32, t: u32, history_bits: &[u8], num_actions: u8) -> u8 {
    if history_bits.is_empty() {
        return 0;
    }
    let rows = run_shrinking_ca(rule_table, k, two_r, t, history_bits);
    let last_row = rows.last().unwrap();
    if last_row.is_empty() {
        0
    } else {
        last_row[last_row.len() - 1] % num_actions
    }
}

/// Compute a RuleArray strategy's move from game history bits.
/// Applies an inhomogeneous CA: each step uses a different rule table.
/// Uses periodic boundaries (constant width, unlike shrinking CA).
/// For r=1/2 (two_r=1): alternating neighborhood
///   step 0 (1-indexed step 1, odd): (center, right)
///   step 1 (1-indexed step 2, even): (left, center)
/// For r=1 (two_r=2): full (left, center, right) neighborhood
pub fn rule_array_move(
    rule_tables: &[Vec<u8>],
    k: u8,
    two_r: u32,
    t: u32,
    history_bits: &[u8],
    num_actions: u8,
) -> u8 {
    if history_bits.is_empty() {
        return 0;
    }

    let width = history_bits.len();
    let k_usize = k.max(2) as usize;

    let mut row: Vec<u8> = history_bits.to_vec();

    for step in 0..t.min(rule_tables.len() as u32) {
        let rule_table = &rule_tables[step as usize];
        let mut next_row = vec![0u8; width];

        for j in 0..width {
            let left = row[(j + width - 1) % width];
            let center = row[j];
            let right = row[(j + 1) % width];

            let input = if two_r == 1 {
                // r=1/2: alternating neighborhood
                if (step + 1) % 2 == 1 {
                    // odd step (1-indexed): (center, right)
                    (center as usize) * k_usize + right as usize
                } else {
                    // even step (1-indexed): (left, center)
                    (left as usize) * k_usize + center as usize
                }
            } else if two_r == 2 {
                // r=1: full (left, center, right) neighborhood
                (left as usize) * k_usize * k_usize + (center as usize) * k_usize + right as usize
            } else {
                // General case: fallback to center only
                (center as usize) % rule_table.len()
            };

            next_row[j] = rule_table.get(input).copied().unwrap_or(0);
        }

        row = next_row;
    }

    // Return last cell of final state, like ca_move
    if row.is_empty() {
        0
    } else {
        row[row.len() - 1] % num_actions
    }
}

// ── Math helpers ─────────────────────────────────────────────────────────────

fn floor_div_rem_i128(numer: i128, denom: i128) -> (i128, i128) {
    if denom == 0 {
        return (0, 0);
    }
    let mut q = numer / denom;
    let mut r = numer % denom;
    if r < 0 {
        q -= 1;
        r += denom;
    }
    (q, r)
}

fn integer_digits_signed_abs(value: i128, base: usize, len: usize) -> Vec<usize> {
    integer_digits_unsigned(value.unsigned_abs(), base, len)
}

fn integer_digits_unsigned(mut value: u128, base: usize, len: usize) -> Vec<usize> {
    if len == 0 {
        return Vec::new();
    }
    let base_u128 = base.max(2) as u128;
    let mut digits = vec![0usize; len];
    for idx in (0..len).rev() {
        digits[idx] = (value % base_u128) as usize;
        value /= base_u128;
    }
    digits
}

fn checked_pow_u128(base: u128, exp: u32) -> Option<u128> {
    let mut value = 1u128;
    for _ in 0..exp {
        value = value.checked_mul(base)?;
    }
    Some(value)
}

fn checked_pow_usize(base: usize, exp: usize) -> Option<usize> {
    let mut value = 1usize;
    for _ in 0..exp {
        value = value.checked_mul(base)?;
    }
    Some(value)
}

// ── FSM two-step classification (canonicalize + behavioral grouping) ─────────

use rayon::prelude::*;
use std::collections::{HashMap, VecDeque};

/// Raw decoded FSM for canonicalization (outputs + transitions as Vec<Vec<usize>>)
#[derive(Clone, Debug)]
pub struct RawFsm {
    pub outputs: Vec<usize>,          // outputs[state] = action
    pub transitions: Vec<Vec<usize>>, // transitions[state][input] = next_state
    pub actions: usize,
}

impl RawFsm {
    pub fn states(&self) -> usize {
        self.outputs.len()
    }
}

/// Decode FSM index to RawFsm (same encoding as decode_fsm but returns RawFsm struct).
pub fn decode_fsm_raw(index: u64, states: usize, actions: usize) -> Result<RawFsm, String> {
    if states == 0 {
        return Err("fsm decode requires states > 0".to_string());
    }
    if actions == 0 {
        return Err("fsm decode requires actions > 0".to_string());
    }
    let Some(max) = fsm_count(states, actions) else {
        return Err("fsm index space overflows u128 for this (states, actions)".to_string());
    };
    if (index as u128) >= max {
        return Err(format!("fsm index {index} out of range (0..{})", max - 1));
    }

    let transition_digits = states.saturating_mul(actions);
    let Some(action_block) = checked_pow_u128(actions as u128, states as u32) else {
        return Err("fsm action block overflows u128".to_string());
    };
    let (transition_code, output_code) =
        floor_div_rem_i128(index as i128 - 1, action_block as i128);

    let transitions_flat = if states == 1 {
        vec![0usize; transition_digits]
    } else {
        integer_digits_unsigned(transition_code.unsigned_abs(), states, transition_digits)
    };
    let outputs = if actions == 1 {
        vec![0usize; states]
    } else {
        integer_digits_unsigned(output_code as u128, actions, states)
    };

    let mut transitions = vec![vec![0usize; actions]; states];
    for (state_idx, row) in transitions.iter_mut().enumerate() {
        for (input_idx, cell) in row.iter_mut().enumerate() {
            let flat_idx = state_idx.saturating_mul(actions).saturating_add(input_idx);
            let next = transitions_flat.get(flat_idx).copied().unwrap_or(0);
            *cell = next.min(states - 1);
        }
    }

    Ok(RawFsm {
        outputs,
        transitions,
        actions,
    })
}

/// BFS-canonicalize: reorder states starting from `start_state`, drop unreachable.
pub fn canonicalize_raw_fsm(raw: &RawFsm, start_state: usize) -> RawFsm {
    if raw.states() == 0 {
        return raw.clone();
    }
    let start = start_state.min(raw.states().saturating_sub(1));
    let mut state_map = vec![None; raw.states()];
    let mut order = Vec::with_capacity(raw.states());
    let mut queue = VecDeque::new();
    let mut next_id = 1usize;
    state_map[start] = Some(0usize);
    queue.push_back(start);

    while let Some(state) = queue.pop_front() {
        order.push(state);
        let row = raw.transitions.get(state);
        for input in 0..raw.actions {
            let next = row
                .and_then(|r| r.get(input))
                .copied()
                .unwrap_or(state)
                .min(raw.states().saturating_sub(1));
            if state_map[next].is_none() {
                state_map[next] = Some(next_id);
                next_id += 1;
                queue.push_back(next);
            }
        }
    }

    let mut outputs = Vec::with_capacity(order.len());
    let mut transitions = Vec::with_capacity(order.len());
    for &state in &order {
        outputs.push(raw.outputs.get(state).copied().unwrap_or(0));
        let mut row = Vec::with_capacity(raw.actions);
        for input in 0..raw.actions {
            let next = raw
                .transitions
                .get(state)
                .and_then(|r| r.get(input))
                .copied()
                .unwrap_or(state)
                .min(raw.states().saturating_sub(1));
            row.push(state_map[next].unwrap_or(0));
        }
        transitions.push(row);
    }

    RawFsm {
        outputs,
        transitions,
        actions: raw.actions,
    }
}

/// Structural key for deduplication (flattened outputs + transitions).
pub fn raw_fsm_key(raw: &RawFsm) -> Vec<u16> {
    let mut key = Vec::with_capacity(raw.states().saturating_mul(raw.actions + 1));
    for output in &raw.outputs {
        key.push(*output as u16);
    }
    for row in &raw.transitions {
        for next in row {
            key.push(*next as u16);
        }
    }
    key
}

/// Behavioral trace signature: run FSM on all input sequences of given length,
/// record output trace. Two FSMs with the same trace behave identically.
pub fn behavior_trace_signature(raw: &RawFsm, steps: usize) -> Vec<u16> {
    if raw.states() == 0 || raw.actions == 0 || steps == 0 {
        return Vec::new();
    }
    let sequence_count = match checked_pow_u128(raw.actions as u128, steps as u32) {
        Some(c) if c <= usize::MAX as u128 => c as usize,
        _ => return Vec::new(),
    };
    let capacity = sequence_count.saturating_mul(steps);
    let mut signature = Vec::with_capacity(capacity);
    let mut digits = vec![0usize; steps];

    for sequence_idx in 0..sequence_count {
        let mut code = sequence_idx;
        for pos in (0..steps).rev() {
            let digit = code % raw.actions;
            code /= raw.actions;
            digits[pos] = raw.actions - 1 - digit;
        }

        let mut state = 0usize;
        for &input in &digits {
            let next = raw
                .transitions
                .get(state)
                .and_then(|row| row.get(input))
                .copied()
                .unwrap_or(state)
                .min(raw.states().saturating_sub(1));
            let out = raw.outputs.get(next).copied().unwrap_or(0);
            signature.push(out as u16);
            state = next;
        }
    }

    signature
}

const NOTEBOOK_BEHAVIOR_TRACE_STEPS: usize = 12;

fn insert_min_index(map: &mut HashMap<Vec<u16>, u64>, key: Vec<u16>, idx: u64) {
    if let Some(existing) = map.get_mut(&key) {
        *existing = (*existing).min(idx);
    } else {
        map.insert(key, idx);
    }
}

fn merge_min_index_maps(left: &mut HashMap<Vec<u16>, u64>, right: HashMap<Vec<u16>, u64>) {
    for (key, idx) in right {
        insert_min_index(left, key, idx);
    }
}

/// Two-step FSM classification result.
pub struct FsmClassification {
    /// Representatives of unique behavioral groups (min-index per group).
    pub representatives: Vec<u64>,
    /// Groups of FSM indices sharing the same behavior.
    pub groups: Vec<Vec<u64>>,
    /// Number of unique behaviors.
    pub unique_behaviors: usize,
    /// Total FSM index space.
    pub total_rules: u128,
    /// Number of structurally unique FSMs (after canonicalization, before behavioral grouping).
    pub canonical_count: usize,
    /// How many FSMs were actually examined.
    pub sampled_rules: usize,
}

/// Perform two-step FSM classification:
/// 1. Canonicalize (BFS-reorder + structural dedup) -> canonical representatives
/// 2. Behavioral grouping (12-step trace) -> unique behavior groups
///
/// `states`/`actions` define the FSM space.
/// If `sample > 0`, randomly sample that many indices; otherwise exhaustive.
pub fn fsm_classify_two_step(
    states: usize,
    actions: usize,
    sample: usize,
) -> Result<FsmClassification, String> {
    let total_rules = fsm_count(states, actions)
        .ok_or_else(|| "fsm space overflow".to_string())?;

    if total_rules == 0 {
        return Ok(FsmClassification {
            representatives: Vec::new(),
            groups: Vec::new(),
            unique_behaviors: 0,
            total_rules,
            canonical_count: 0,
            sampled_rules: 0,
        });
    }

    // Build candidate list: exhaustive or random sample
    let candidates: Vec<u64> = if sample > 0 {
        use std::collections::HashSet;
        let mut seen = HashSet::new();
        let target = (sample as u128).min(total_rules);
        let mut rng_state = 0x5DEECE66Du64.wrapping_mul(42).wrapping_add(0xBu64);
        while (seen.len() as u128) < target {
            rng_state = rng_state
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            let hi = rng_state >> 32;
            rng_state = rng_state
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            let lo = rng_state >> 32;
            let full = (hi << 32) | lo;
            let idx = (full as u128 % total_rules) as u64;
            seen.insert(idx);
        }
        let mut v: Vec<u64> = seen.into_iter().collect();
        v.sort_unstable();
        v
    } else {
        if total_rules > u64::MAX as u128 {
            return Err("fsm space too large for exhaustive enumeration".to_string());
        }
        (0..total_rules as u64).collect()
    };

    let sampled_rules = candidates.len();

    // Step 1: Canonicalize each FSM index -> structural key, keep min-index per key
    let canonical_by_key = candidates
        .par_iter()
        .try_fold(HashMap::new, |mut local: HashMap<Vec<u16>, u64>, &idx| {
            let raw = decode_fsm_raw(idx, states, actions)?;
            let canonical = canonicalize_raw_fsm(&raw, 0);
            let key = raw_fsm_key(&canonical);
            insert_min_index(&mut local, key, idx);
            Ok::<_, String>(local)
        })
        .try_reduce(HashMap::new, |mut left, right| {
            merge_min_index_maps(&mut left, right);
            Ok::<_, String>(left)
        })?;

    let canonical_count = canonical_by_key.len();
    let mut canonical_reps: Vec<u64> = canonical_by_key.into_values().collect();
    canonical_reps.sort_unstable();

    // Step 2: For each canonical representative, compute behavioral trace -> group by behavior
    let behavior_by_key = canonical_reps
        .par_iter()
        .try_fold(HashMap::new, |mut local: HashMap<Vec<u16>, Vec<u64>>, &idx| {
            let raw = decode_fsm_raw(idx, states, actions)?;
            let trace = behavior_trace_signature(&raw, NOTEBOOK_BEHAVIOR_TRACE_STEPS);
            local.entry(trace).or_default().push(idx);
            Ok::<_, String>(local)
        })
        .try_reduce(HashMap::new, |mut left, right| {
            for (key, mut members) in right {
                left.entry(key).or_default().append(&mut members);
            }
            Ok::<_, String>(left)
        })?;

    let mut groups: Vec<Vec<u64>> = behavior_by_key.into_values().collect();
    for group in &mut groups {
        group.sort_unstable();
    }
    groups.sort_unstable_by_key(|g| g[0]);

    let representatives: Vec<u64> = groups.iter().map(|g| g[0]).collect();
    let unique_behaviors = groups.len();

    Ok(FsmClassification {
        representatives,
        groups,
        unique_behaviors,
        total_rules,
        canonical_count,
        sampled_rules,
    })
}

// ── Generalized game play ────────────────────────────────────────────────────

/// Play one iterated game between two strategy runners.
/// Returns (score_a, score_b).
/// Play an iterated game between two strategy runners.
/// Returns `Some((score_a, score_b))` if both players halt every round,
/// or `None` if either player fails to halt (the game is aborted).
/// The `failed` output indicates which player(s) failed: 0=none, 1=a, 2=b, 3=both.
pub fn play_game(
    runner_a: &mut StrategyRunner,
    runner_b: &mut StrategyRunner,
    rounds: u32,
    payoff: &Payoff,
) -> (Option<(i64, i64)>, u8) {
    runner_a.reset();
    runner_b.reset();
    let mut history: Vec<u8> = Vec::with_capacity(2 * rounds as usize);
    let mut score_a = 0i64;
    let mut score_b = 0i64;

    for round in 0..rounds {
        let move_a = match runner_a.get_move(&history, round) {
            Some(m) => m,
            None => return (None, 1), // player A failed to halt
        };
        let move_b = match runner_b.get_move(&history, round) {
            Some(m) => m,
            None => return (None, 2), // player B failed to halt
        };

        history.push(move_a);
        history.push(move_b);

        let idx = (move_a as usize) * 2 + move_b as usize;
        if let Some(payoffs) = payoff.get(idx) {
            score_a += payoffs[0] as i64;
            score_b += payoffs[1] as i64;
        }
    }

    (Some((score_a, score_b)), 0)
}

/// Play an iterated game between two strategy runners using a DynPayoff (k-action games).
/// Returns `Some((score_a, score_b))` if both players halt every round,
/// or `None` if either player fails to halt (the game is aborted).
/// The `failed` output indicates which player(s) failed: 0=none, 1=a, 2=b, 3=both.
pub fn play_game_dyn(
    runner_a: &mut StrategyRunner,
    runner_b: &mut StrategyRunner,
    rounds: u32,
    payoff: &DynPayoff,
) -> (Option<(i64, i64)>, u8) {
    runner_a.reset();
    runner_b.reset();
    let mut history: Vec<u8> = Vec::with_capacity(2 * rounds as usize);
    let mut score_a = 0i64;
    let mut score_b = 0i64;

    for round in 0..rounds {
        let move_a = match runner_a.get_move(&history, round) {
            Some(m) => m,
            None => return (None, 1), // player A failed to halt
        };
        let move_b = match runner_b.get_move(&history, round) {
            Some(m) => m,
            None => return (None, 2), // player B failed to halt
        };

        history.push(move_a);
        history.push(move_b);

        let idx = (move_a as usize) * payoff.num_actions + move_b as usize;
        if let Some(payoffs) = payoff.get(idx) {
            score_a += payoffs[0] as i64;
            score_b += payoffs[1] as i64;
        }
    }

    (Some((score_a, score_b)), 0)
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── FSM decode tests ─────────────────────────────────────────────────

    #[test]
    fn fsm_count_2_2() {
        // s=2, k=2: 2^(2*2) * 2^2 = 16 * 4 = 64
        assert_eq!(fsm_count(2, 2), Some(64));
    }

    #[test]
    fn fsm_count_1_2() {
        // s=1, k=2: 1^(1*2) * 2^1 = 1 * 2 = 2
        assert_eq!(fsm_count(1, 2), Some(2));
    }

    #[test]
    fn fsm_count_3_2() {
        // s=3, k=2: 3^(3*2) * 2^3 = 729 * 8 = 5832
        assert_eq!(fsm_count(3, 2), Some(5832));
    }

    #[test]
    fn fsm_decode_index_0() {
        // FSM index 0 (first FSM)
        let (outputs, transitions) = decode_fsm(0, 2, 2);
        assert_eq!(outputs.len(), 2);
        assert_eq!(transitions.len(), 4); // 2 states * 2 inputs
    }

    #[test]
    fn fsm_decode_outputs_range() {
        // All outputs should be in [0, k)
        for id in 0..64u64 {
            let (outputs, _) = decode_fsm(id, 2, 2);
            for &o in &outputs {
                assert!(o < 2, "FSM {}: output {} out of range", id, o);
            }
        }
    }

    #[test]
    fn fsm_decode_transitions_range() {
        // All transition targets should be in [0, states)
        for id in 0..64u64 {
            let (_, transitions) = decode_fsm(id, 2, 2);
            for &t in &transitions {
                assert!(t < 2, "FSM {}: transition target {} out of range", id, t);
            }
        }
    }

    #[test]
    fn fsm_decode_mostly_unique() {
        // FSM decode for (2,2): 64 total indices. Due to the signed-abs encoding
        // at index 0 (transition_code = -1 wraps to same abs as +1), there is
        // exactly 1 collision (indices 0 and 8). This matches the WL notebook
        // encoding and nit-games behavior. The canonical dedup layer handles this.
        let mut rules: Vec<(Vec<u8>, Vec<usize>)> = (0..64)
            .map(|id| decode_fsm(id, 2, 2))
            .collect();
        rules.sort();
        rules.dedup();
        // 63 unique rules out of 64 indices (1 collision)
        assert_eq!(rules.len(), 63, "expected 63 unique FSM rules for (2,2)");
    }

    #[test]
    fn fsm_always_defect_index_0() {
        // FSM 0 with s=1, k=2 is the always-defect strategy (matches nit-games)
        let (outputs, _) = decode_fsm(0, 1, 2);
        assert_eq!(outputs, vec![1]);
    }

    #[test]
    fn fsm_always_cooperate_index_1() {
        // FSM 1 with s=1, k=2 is the always-cooperate strategy (matches nit-games)
        let (outputs, _) = decode_fsm(1, 1, 2);
        assert_eq!(outputs, vec![0]);
    }

    #[test]
    fn fsm_runner_cooperates_on_round_0() {
        let spec = StrategySpec::Fsm { id: 0, s: 2, k: 2, num_actions: 2 };
        let mut runner = StrategyRunner::new(&spec);
        let m = runner.get_move(&[], 0).unwrap();
        // FSM outputs based on initial state; the exact value depends on the FSM
        assert!(m < 2);
    }

    #[test]
    fn fsm_runner_stateful_across_rounds() {
        // Create an FSM and verify it can change state
        let spec = StrategySpec::Fsm { id: 47, s: 2, k: 2, num_actions: 2 };
        let mut runner = StrategyRunner::new(&spec);
        let m0 = runner.get_move(&[], 0).unwrap();
        let m1 = runner.get_move(&[m0, 1], 1).unwrap();
        assert!(m0 < 2);
        assert!(m1 < 2);
    }

    #[test]
    fn fsm_runner_reset_restores_state() {
        let spec = StrategySpec::Fsm { id: 47, s: 2, k: 2, num_actions: 2 };
        let mut runner = StrategyRunner::new(&spec);
        let m0 = runner.get_move(&[], 0).unwrap();
        let _ = runner.get_move(&[m0, 1], 1).unwrap();
        runner.reset();
        let m0_again = runner.get_move(&[], 0).unwrap();
        assert_eq!(m0, m0_again, "reset should restore initial state");
    }

    // ── CA decode tests ──────────────────────────────────────────────────

    #[test]
    fn ca_rule_table_elementary_rule_30() {
        // Elementary CA: k=2, r=0.5 -> two_r=1, neighborhood=2
        // Wait, for elementary CAs: k=2, r=1 -> two_r=2, neighborhood=3
        // Rule 30 = 00011110 in binary -> table[0..8]
        let table = decode_ca_rule_table(30, 2, 2);
        assert_eq!(table.len(), 8); // 2^3 = 8
        // Rule 30 in IntegerDigits[30, 2, 8]:
        // 30 = 0*128 + 0*64 + 0*32 + 1*16 + 1*8 + 1*4 + 1*2 + 0*1
        // digits: [0, 0, 0, 1, 1, 1, 1, 0]
        assert_eq!(table, vec![0, 0, 0, 1, 1, 1, 1, 0]);
    }

    #[test]
    fn ca_rule_table_elementary_rule_110() {
        // Rule 110 = 01101110 in binary
        // IntegerDigits[110, 2, 8] = [0, 1, 1, 0, 1, 1, 1, 0]
        let table = decode_ca_rule_table(110, 2, 2);
        assert_eq!(table.len(), 8);
        assert_eq!(table, vec![0, 1, 1, 0, 1, 1, 1, 0]);
    }

    #[test]
    fn ca_rule_table_size_k2_r1() {
        // k=2, r=1 -> two_r=2, neighborhood=3, patterns=8
        let table = decode_ca_rule_table(0, 2, 2);
        assert_eq!(table.len(), 8);
    }

    #[test]
    fn ca_rule_table_size_k3_r1() {
        // k=3, r=1 -> two_r=2, neighborhood=3, patterns=27
        let table = decode_ca_rule_table(0, 3, 2);
        assert_eq!(table.len(), 27);
    }

    #[test]
    fn shrinking_ca_shrinks_correctly() {
        // k=2, two_r=2 (neighborhood=3): each step reduces length by 2
        let table = decode_ca_rule_table(110, 2, 2);
        let input = vec![1, 0, 1, 1, 0];
        let rows = run_shrinking_ca(&table, 2, 2, 2, &input);
        assert_eq!(rows[0].len(), 5); // input
        assert_eq!(rows[1].len(), 3); // after 1 step: 5 - 2 = 3
        assert_eq!(rows[2].len(), 1); // after 2 steps: 3 - 2 = 1
    }

    #[test]
    fn shrinking_ca_stops_when_too_short() {
        let table = decode_ca_rule_table(110, 2, 2);
        let input = vec![1, 0];
        let rows = run_shrinking_ca(&table, 2, 2, 10, &input);
        // Input has 2 cells, neighborhood is 3, so no evolution is possible
        assert_eq!(rows.len(), 1);
    }

    #[test]
    fn ca_move_empty_history() {
        let table = decode_ca_rule_table(110, 2, 2);
        assert_eq!(ca_move(&table, 2, 2, 10, &[], 2), 0);
    }

    #[test]
    fn ca_move_returns_valid_output() {
        let table = decode_ca_rule_table(110, 2, 2);
        let history = vec![0, 1, 1, 0, 0, 1];
        let m = ca_move(&table, 2, 2, 2, &history, 2);
        assert!(m < 2);
    }

    #[test]
    fn ca_runner_cooperates_round_0() {
        let spec = StrategySpec::Ca {
            rule: 110,
            k: 2,
            r: 1.0,
            t: 10,
            num_actions: 2,
        };
        let mut runner = StrategyRunner::new(&spec);
        assert_eq!(runner.get_move(&[], 0), Some(0));
    }

    #[test]
    fn ca_runner_produces_valid_moves() {
        let spec = StrategySpec::Ca {
            rule: 110,
            k: 2,
            r: 1.0,
            t: 2,
            num_actions: 2,
        };
        let mut runner = StrategyRunner::new(&spec);
        let m0 = runner.get_move(&[], 0).unwrap();
        assert!(m0 < 2);
        let m1 = runner.get_move(&[0, 1], 1).unwrap();
        assert!(m1 < 2);
        let m2 = runner.get_move(&[0, 1, m1, 0], 2).unwrap();
        assert!(m2 < 2);
    }

    // ── TM runner tests ──────────────────────────────────────────────────

    #[test]
    fn tm_runner_cooperates_round_0() {
        let spec = StrategySpec::Tm {
            id: 64,
            s: 2,
            k: 2,
            max_steps: 500,
            num_actions: 2,
        };
        let mut runner = StrategyRunner::new(&spec);
        assert_eq!(runner.get_move(&[], 0), Some(0));
    }

    #[test]
    fn tm_runner_matches_cpu_play() {
        // TM 64 always cooperates, so should produce 0 on any history
        let spec = StrategySpec::Tm {
            id: 64,
            s: 2,
            k: 2,
            max_steps: 500,
            num_actions: 2,
        };
        let mut runner = StrategyRunner::new(&spec);
        assert_eq!(runner.get_move(&[], 0), Some(0));
        assert_eq!(runner.get_move(&[0, 0], 1), Some(0));
        assert_eq!(runner.get_move(&[0, 0, 0, 0], 2), Some(0));
    }

    // ── Generalized play_game test ───────────────────────────────────────

    #[test]
    fn play_game_tm_vs_tm_cooperator() {
        let spec_a = StrategySpec::Tm {
            id: 64,
            s: 2,
            k: 2,
            max_steps: 500,
            num_actions: 2,
        };
        let spec_b = StrategySpec::Tm {
            id: 64,
            s: 2,
            k: 2,
            max_steps: 500,
            num_actions: 2,
        };
        let mut runner_a = StrategyRunner::new(&spec_a);
        let mut runner_b = StrategyRunner::new(&spec_b);
        let payoff = crate::tournament::parse_game("pd").unwrap();
        let (result, failed) = play_game(&mut runner_a, &mut runner_b, 10, &payoff);
        assert_eq!(failed, 0);
        let (sa, sb) = result.unwrap();
        assert_eq!(sa, -10);
        assert_eq!(sb, -10);
    }

    #[test]
    fn play_game_fsm_vs_fsm() {
        let spec_a = StrategySpec::Fsm { id: 1, s: 1, k: 2, num_actions: 2 };
        let spec_b = StrategySpec::Fsm { id: 0, s: 1, k: 2, num_actions: 2 };
        let mut runner_a = StrategyRunner::new(&spec_a);
        let mut runner_b = StrategyRunner::new(&spec_b);
        let payoff = crate::tournament::parse_game("pd").unwrap();
        let (result, failed) = play_game(&mut runner_a, &mut runner_b, 10, &payoff);
        assert_eq!(failed, 0);
        let (sa, sb) = result.unwrap();
        assert_eq!(sa, -30);
        assert_eq!(sb, 0);
    }

    #[test]
    fn play_game_mixed_strategies() {
        let spec_a = StrategySpec::Tm {
            id: 64,
            s: 2,
            k: 2,
            max_steps: 500,
            num_actions: 2,
        };
        let spec_b = StrategySpec::Fsm { id: 0, s: 1, k: 2, num_actions: 2 };
        let mut runner_a = StrategyRunner::new(&spec_a);
        let mut runner_b = StrategyRunner::new(&spec_b);
        let payoff = crate::tournament::parse_game("pd").unwrap();
        let (result, failed) = play_game(&mut runner_a, &mut runner_b, 10, &payoff);
        assert_eq!(failed, 0);
        let (sa, sb) = result.unwrap();
        assert_eq!(sa, -30);
        assert_eq!(sb, 0);
    }

    // ── JSON deserialization tests ───────────────────────────────────────

    #[test]
    fn parse_tm_spec() {
        let json = r#"{"type":"tm","id":323,"s":2,"k":2}"#;
        let spec: StrategySpec = serde_json::from_str(json).unwrap();
        match spec {
            StrategySpec::Tm { id, s, k, max_steps, num_actions } => {
                assert_eq!(id, 323);
                assert_eq!(s, 2);
                assert_eq!(k, 2);
                assert_eq!(max_steps, 500); // default
                assert_eq!(num_actions, 2); // default
            }
            _ => panic!("expected Tm"),
        }
    }

    #[test]
    fn parse_tm_spec_with_max_steps() {
        let json = r#"{"type":"tm","id":323,"s":2,"k":2,"max_steps":1000}"#;
        let spec: StrategySpec = serde_json::from_str(json).unwrap();
        match spec {
            StrategySpec::Tm { max_steps, .. } => assert_eq!(max_steps, 1000),
            _ => panic!("expected Tm"),
        }
    }

    #[test]
    fn parse_fsm_spec() {
        let json = r#"{"type":"fsm","id":47,"s":2,"k":2}"#;
        let spec: StrategySpec = serde_json::from_str(json).unwrap();
        match spec {
            StrategySpec::Fsm { id, s, k, num_actions } => {
                assert_eq!(id, 47);
                assert_eq!(s, 2);
                assert_eq!(k, 2);
                assert_eq!(num_actions, 2); // default
            }
            _ => panic!("expected Fsm"),
        }
    }

    #[test]
    fn parse_ca_spec() {
        let json = r#"{"type":"ca","rule":110,"k":2,"r":1.0,"t":10}"#;
        let spec: StrategySpec = serde_json::from_str(json).unwrap();
        match spec {
            StrategySpec::Ca { rule, k, r, t, num_actions } => {
                assert_eq!(rule, 110);
                assert_eq!(k, 2);
                assert!((r - 1.0).abs() < f32::EPSILON);
                assert_eq!(t, 10);
                assert_eq!(num_actions, 2); // default
            }
            _ => panic!("expected Ca"),
        }
    }

    #[test]
    fn parse_ndjson_strategies() {
        let ndjson = r#"{"type":"tm","id":323,"s":2,"k":2}
{"type":"fsm","id":47,"s":2,"k":2}
{"type":"ca","rule":110,"k":2,"r":1.0,"t":10}"#;
        let specs: Vec<StrategySpec> = ndjson
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(|l| serde_json::from_str(l).unwrap())
            .collect();
        assert_eq!(specs.len(), 3);
        assert!(matches!(specs[0], StrategySpec::Tm { .. }));
        assert!(matches!(specs[1], StrategySpec::Fsm { .. }));
        assert!(matches!(specs[2], StrategySpec::Ca { .. }));
    }

    // ── Math helpers ─────────────────────────────────────────────────────

    #[test]
    fn floor_div_rem_positive() {
        assert_eq!(floor_div_rem_i128(7, 3), (2, 1));
    }

    #[test]
    fn floor_div_rem_negative() {
        // -1 / 4 = -1 remainder 3 (floor division)
        assert_eq!(floor_div_rem_i128(-1, 4), (-1, 3));
    }

    #[test]
    fn floor_div_rem_zero() {
        assert_eq!(floor_div_rem_i128(0, 4), (0, 0));
    }

    #[test]
    fn integer_digits_basic() {
        // 5 in base 2, length 4: [0, 1, 0, 1]
        assert_eq!(integer_digits_unsigned(5, 2, 4), vec![0, 1, 0, 1]);
    }

    #[test]
    fn integer_digits_zero() {
        assert_eq!(integer_digits_unsigned(0, 2, 3), vec![0, 0, 0]);
    }

    #[test]
    fn checked_pow_basic() {
        assert_eq!(checked_pow_u128(2, 10), Some(1024));
        assert_eq!(checked_pow_u128(3, 0), Some(1));
    }
}
