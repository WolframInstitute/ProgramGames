//! Strategy module: unified TM, FSM, and CA strategy runners.
//!
//! Provides a `StrategySpec` enum (JSON-deserializable) and a `StrategyRunner`
//! that can play any of the three strategy types in an iterated game.

use serde::Deserialize;

use crate::tournament::{DynPayoff, Payoff};
use crate::TmTransition;

// ── Flexible ID deserializer (accepts JSON number or string) ───────────────

fn deserialize_big_id<'de, D>(deserializer: D) -> Result<String, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct IdVisitor;
    impl<'de> serde::de::Visitor<'de> for IdVisitor {
        type Value = String;
        fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("a number or numeric string")
        }
        fn visit_u64<E: serde::de::Error>(self, v: u64) -> Result<String, E> {
            Ok(v.to_string())
        }
        fn visit_i64<E: serde::de::Error>(self, v: i64) -> Result<String, E> {
            Ok(v.to_string())
        }
        fn visit_f64<E: serde::de::Error>(self, v: f64) -> Result<String, E> {
            Ok((v as u64).to_string())
        }
        fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<String, E> {
            Ok(v.to_string())
        }
        fn visit_string<E: serde::de::Error>(self, v: String) -> Result<String, E> {
            Ok(v)
        }
    }
    deserializer.deserialize_any(IdVisitor)
}

// ── Bigint-style base conversion from decimal string ───────────────────────

/// Divide a decimal digit vector by `divisor`, returning the remainder.
fn div_decimal_vec(num: &mut Vec<u8>, divisor: u64) -> u64 {
    let mut remainder = 0u128;
    for d in num.iter_mut() {
        remainder = remainder * 10 + *d as u128;
        *d = (remainder / divisor as u128) as u8;
        remainder %= divisor as u128;
    }
    // Strip leading zeros
    while num.len() > 1 && num[0] == 0 {
        num.remove(0);
    }
    remainder as u64
}

/// Convert a decimal string to digits in the given base (MSD first).
fn string_to_base_digits(id_str: &str, base: u64, len: usize) -> Vec<u64> {
    let mut decimal: Vec<u8> = id_str.bytes().map(|b| b - b'0').collect();
    let mut digits = vec![0u64; len];
    for i in (0..len).rev() {
        digits[i] = div_decimal_vec(&mut decimal, base);
    }
    digits
}

/// Decode a TM from a decimal string ID (supports arbitrarily large IDs).
fn decode_tm_from_str(id_str: &str, states: u16, symbols: u8) -> Vec<TmTransition> {
    // Fast path for ids that fit in u64
    if let Ok(code) = id_str.parse::<u64>() {
        return crate::decode_tm(code, states, symbols);
    }

    let s = states as usize;
    let k = symbols as usize;
    let total = s * k;
    let base = (k * s * 2) as u64;

    let mut transitions = vec![
        TmTransition { write: 0, move_right: false, next: 1 };
        total
    ];
    if base == 0 {
        return transitions;
    }

    let mut decimal: Vec<u8> = id_str.bytes().map(|b| b - b'0').collect();
    for state in (1..=s).rev() {
        for read in 0..k {
            let digit = div_decimal_vec(&mut decimal, base);
            let move_right = (digit % 2) == 1;
            let write = ((digit / 2) % k as u64) as u8;
            let next = (digit / (2 * k as u64)) as u16 + 1;
            let idx = (state - 1) * k + read;
            transitions[idx] = TmTransition { write, move_right, next };
        }
    }
    transitions
}

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
        #[serde(deserialize_with = "deserialize_big_id")]
        id: String,
        s: u16,
        k: u8,
        #[serde(default = "default_max_steps")]
        max_steps: u32,
        #[serde(default = "default_num_actions")]
        num_actions: u8,
    },
    #[serde(rename = "fsm")]
    Fsm {
        #[serde(deserialize_with = "deserialize_big_id")]
        id: String,
        s: u16,
        k: u8,
        #[serde(default = "default_num_actions")]
        num_actions: u8,
    },
    #[serde(rename = "ca")]
    Ca {
        #[serde(deserialize_with = "deserialize_big_id")]
        rule: String,
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
    /// Override num_actions on this spec.
    pub fn set_num_actions(&mut self, na: u8) {
        match self {
            StrategySpec::Tm { num_actions, .. }
            | StrategySpec::Fsm { num_actions, .. }
            | StrategySpec::Ca { num_actions, .. }
            | StrategySpec::RuleArray { num_actions, .. } => {
                *num_actions = na;
            }
        }
    }

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
                let transitions = decode_tm_from_str(id, *s, *k);
                RunnerInner::Tm {
                    transitions,
                    symbols: *k,
                    max_steps: *max_steps,
                    num_actions: *num_actions,
                }
            }
            StrategySpec::Fsm { id, s, k, num_actions } => {
                let (outputs, transitions) =
                    decode_fsm_from_str(id, *s as usize, *k as usize);
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
                let rule_table = decode_ca_rule_table_from_str(rule, *k, two_r);
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
                // Strip leading zeros from history (equivalent to WL FromDigits→IntegerDigits)
                // and run TM with the result as tape. Avoids u64 overflow for long histories.
                let start = history.iter().position(|&d| d != 0).unwrap_or(history.len());
                let digits: Vec<u8> = if start >= history.len() {
                    vec![0u8]
                } else {
                    history[start..].to_vec()
                };
                let k = (*symbols).max(2);
                let mut tape = digits;
                let mut head: usize = tape.len().saturating_sub(1);
                let mut tm_state: u16 = 1;
                let mut halted = false;
                let mut output = 0u8;
                for _step in 0..*max_steps {
                    let read = tape.get(head).copied().unwrap_or(0);
                    let idx = (tm_state.saturating_sub(1) as usize) * (k as usize) + (read as usize);
                    let Some(&trans) = transitions.get(idx) else { break; };
                    if let Some(cell) = tape.get_mut(head) { *cell = trans.write; }
                    if trans.move_right && head + 1 == tape.len() {
                        output = tape.last().copied().unwrap_or(0) % 2;
                        halted = true;
                        break;
                    }
                    if trans.move_right {
                        if head + 1 < tape.len() { head += 1; }
                    } else if head == 0 {
                        tape.insert(0, 0);
                    } else {
                        head -= 1;
                    }
                    tm_state = trans.next;
                    if tm_state == 0 { break; }
                }
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
                if round == 0 {
                    // Round 0: output from initial state, no transition (matches WL)
                    let output = outputs.get(*state).copied().unwrap_or(0) % *num_actions;
                    Some(output)
                } else {
                    // Round 1+: transition first, then output
                    let input = history.last().copied().unwrap_or(0) as usize;
                    let idx = (*state) * (*k) + input.min(*k - 1);
                    *state = transitions.get(idx).copied().unwrap_or(0);
                    let output = outputs.get(*state).copied().unwrap_or(0) % *num_actions;
                    Some(output)
                }
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
                if round == 0 {
                    // Round 0: output from initial state, no transition (matches WL)
                    let output = outputs.get(*state).copied().unwrap_or(0) % *num_actions;
                    Some(output)
                } else {
                    // Round 1+: transition first using opponent's last action, then output
                    let input = opponent_last as usize;
                    let idx = (*state) * (*k) + input.min(*k - 1);
                    *state = transitions.get(idx).copied().unwrap_or(0);
                    let output = outputs.get(*state).copied().unwrap_or(0) % *num_actions;
                    Some(output)
                }
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

    /// Returns true if this is an FSM strategy.
    pub fn is_fsm(&self) -> bool {
        matches!(self.inner, RunnerInner::Fsm { .. })
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

/// Decode an FSM from a decimal string ID, supporting IDs that exceed `u64::MAX`.
///
/// For `(s, k)` where `s^(s*k) * k^s` exceeds `u64::MAX` (e.g. s=9, k=2 has ~7.68e19
/// total FSMs, about 4.17x u64::MAX), valid FSM indices can be larger than a u64.
/// This function decomposes the decimal string directly via bigint long division,
/// matching the encoding of `decode_fsm` exactly on the u64 range.
pub fn decode_fsm_from_str(id_str: &str, states: usize, actions: usize) -> (Vec<u8>, Vec<usize>) {
    // Fast path: id fits in u64. Delegates to `decode_fsm` so the signed-abs
    // handling for index=0 is preserved.
    if let Ok(idx) = id_str.parse::<u64>() {
        return decode_fsm(idx, states, actions);
    }

    if states == 0 || actions == 0 {
        return (vec![], vec![]);
    }

    // Any id that fails u64 parsing is > u64::MAX, so (index - 1) is strictly
    // positive — no signed-abs case to worry about.
    let mut decimal: Vec<u8> = id_str
        .bytes()
        .filter(|b| b.is_ascii_digit())
        .map(|b| b - b'0')
        .collect();
    let all_zero = decimal.iter().all(|&d| d == 0);
    if decimal.is_empty() || all_zero {
        return decode_fsm(0, states, actions);
    }

    // Subtract 1 in place.
    let mut i = decimal.len();
    while i > 0 {
        i -= 1;
        if decimal[i] > 0 {
            decimal[i] -= 1;
            break;
        }
        decimal[i] = 9;
    }
    while decimal.len() > 1 && decimal[0] == 0 {
        decimal.remove(0);
    }

    let n = states * actions;

    // Encoding: (index - 1) = transition_code * k^s + output_code.
    // Repeated division by k (s times) peels off the output digits LSD-first;
    // the remaining quotient is the transition_code, which we then peel apart
    // by repeated division by s (n times).
    let mut output_digits = vec![0usize; states];
    if actions > 1 {
        for idx in (0..states).rev() {
            output_digits[idx] = div_decimal_vec(&mut decimal, actions as u64) as usize;
        }
    }

    let mut next_digits = vec![0usize; n];
    if states > 1 {
        for idx in (0..n).rev() {
            next_digits[idx] = div_decimal_vec(&mut decimal, states as u64) as usize;
        }
    }

    let outputs: Vec<u8> = output_digits.into_iter().map(|d| d as u8).collect();

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
/// Matches WL's `CellularAutomaton[{rule, {k, r}}]` encoding.
///
/// The table maps each (2r+1)-neighborhood to an output symbol.
/// WL convention: table[i] = Floor[rule / k^i] mod k (LSD indexing).
/// Index: for neighborhood (d0, d1, ..., d_{2r}), index = d0 + d1*k + ... + d_{2r}*k^{2r}.
pub fn decode_ca_rule_table(rule_code: u64, k: u8, two_r: u32) -> Vec<u8> {
    let neighborhood = two_r as usize + 1;
    let k_usize = k.max(2) as usize;
    let table_len = checked_pow_usize(k_usize, neighborhood).unwrap_or(0);
    let mut table: Vec<u8> = integer_digits_unsigned(rule_code as u128, k_usize, table_len)
        .into_iter()
        .map(|d| d as u8)
        .collect();
    // IntegerDigits gives MSD-first; reverse to get LSD-first (matching WL convention)
    table.reverse();
    table
}

/// Decode a CA rule from a decimal string, supporting rules that exceed `u64::MAX`.
/// Delegates to `decode_ca_rule_table` for ids that fit in u64.
pub fn decode_ca_rule_table_from_str(rule_str: &str, k: u8, two_r: u32) -> Vec<u8> {
    if let Ok(code) = rule_str.parse::<u64>() {
        return decode_ca_rule_table(code, k, two_r);
    }

    let neighborhood = two_r as usize + 1;
    let k_usize = k.max(2) as usize;
    let table_len = checked_pow_usize(k_usize, neighborhood).unwrap_or(0);

    let mut decimal: Vec<u8> = rule_str
        .bytes()
        .filter(|b| b.is_ascii_digit())
        .map(|b| b - b'0')
        .collect();
    if decimal.is_empty() || table_len == 0 {
        return vec![0u8; table_len];
    }

    // Table is stored LSD-first in base k (matches `decode_ca_rule_table` convention).
    let mut table = vec![0u8; table_len];
    for entry in table.iter_mut() {
        *entry = div_decimal_vec(&mut decimal, k_usize as u64) as u8;
    }
    table
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
                // MSD Horner: w0*k^(n-1) + ... + w_{n-1}
                // Combined with LSD-ordered table, matches WL's
                // CellularAutomatonToRule: Reverse[Tuples] -> IntegerDigits
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

            // MSD Horner indexing with LSD table (matches WL CellularAutomatonToRule)
            let input = if two_r == 1 {
                // r=1/2: alternating 2-cell neighborhood
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



/// splitmix64 finalizer. Cheap, portable, good avalanche.
/// Same primitive is used in the MSL GPU kernels so CPU and GPU hashes agree.
#[inline]
pub fn splitmix64(mut x: u64) -> u64 {
    x = x.wrapping_add(0x9E3779B97F4A7C15);
    x = (x ^ (x >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94D049BB133111EB);
    x ^ (x >> 31)
}

/// 128-bit hash of a u16 slice. Two parallel splitmix64 streams with disjoint
/// seeds give effectively independent 64-bit halves. Used to key both the
/// structural canonicalization map (Phase 1) and the behavioral trace map
/// (Phase 2) in `fsm_classify_two_step`. Portable to MSL (see gpu.rs).
///
/// Collision probability at 10^9 distinct inputs ≈ 10^-20 — negligible.
pub fn hash_u16_slice_128(data: &[u16]) -> u128 {
    // Seeds: sqrt(5)/2 and sqrt(2)/2 scaled to u64 (Knuth-style).
    let mut lo: u64 = 0x9E3779B97F4A7C15 ^ (data.len() as u64);
    let mut hi: u64 = 0x6A09E667F3BCC908 ^ (data.len() as u64);
    lo = splitmix64(lo);
    hi = splitmix64(hi);
    for &v in data {
        lo = splitmix64(lo ^ (v as u64));
        hi = splitmix64(hi ^ ((v as u64).wrapping_mul(0x9E3779B97F4A7C15)));
    }
    ((hi as u128) << 64) | (lo as u128)
}

/// Deprecated name kept for any external callers; forwards to the new hash.
#[inline]
pub fn hash_trace_128(trace: &[u16]) -> u128 {
    hash_u16_slice_128(trace)
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

/// CPU Phase 2: behavioral trace hashing + grouping by hash.
/// Used directly when `use_gpu=false`, and as the GPU fallback on any error.
fn trace_hash_cpu(
    canonical_reps: &[u64],
    states: usize,
    actions: usize,
    trace_steps: usize,
) -> Result<HashMap<u128, Vec<u64>>, String> {
    canonical_reps
        .par_iter()
        .try_fold(HashMap::new, |mut local: HashMap<u128, Vec<u64>>, &idx| {
            let raw = decode_fsm_raw(idx, states, actions)?;
            let trace = behavior_trace_signature(&raw, trace_steps);
            let key = hash_u16_slice_128(&trace);
            local.entry(key).or_default().push(idx);
            Ok::<_, String>(local)
        })
        .try_reduce(HashMap::new, |mut left, right| {
            for (key, mut members) in right {
                left.entry(key).or_default().append(&mut members);
            }
            Ok::<_, String>(left)
        })
}

/// CPU Phase 1: structural canonicalization + min-index dedup.
/// Used directly on the sample path, and as the GPU fallback on exhaustive runs.
fn canonicalize_cpu(
    candidates: &[u64],
    states: usize,
    actions: usize,
    full_states_only: bool,
) -> Result<HashMap<u128, u64>, String> {
    candidates
        .par_iter()
        .try_fold(HashMap::new, |mut local: HashMap<u128, u64>, &idx| {
            let raw = decode_fsm_raw(idx, states, actions)?;
            let canonical = canonicalize_raw_fsm(&raw, 0);
            if full_states_only && canonical.states() != states {
                return Ok::<_, String>(local);
            }
            let key_bytes = raw_fsm_key(&canonical);
            let key = hash_u16_slice_128(&key_bytes);
            local
                .entry(key)
                .and_modify(|existing| *existing = (*existing).min(idx))
                .or_insert(idx);
            Ok::<_, String>(local)
        })
        .try_reduce(HashMap::new, |mut left, right| {
            for (key, idx) in right {
                left.entry(key)
                    .and_modify(|existing| *existing = (*existing).min(idx))
                    .or_insert(idx);
            }
            Ok::<_, String>(left)
        })
}

/// Perform two-step FSM classification:
/// 1. Canonicalize (BFS-reorder + structural dedup) -> canonical representatives
/// 2. Behavioral grouping (12-step trace) -> unique behavior groups
///
/// `states`/`actions` define the FSM space.
/// If `sample > 0`, randomly sample that many indices; otherwise exhaustive.
/// If `full_states_only` is true, FSMs whose BFS from the start state does not
/// reach all `states` are discarded before behavioral grouping. This matches the
/// WL `Select[..., VertexCount[FSMGraph[#]] == s &]` filter used to drop FSMs
/// with dead states.
/// If `use_gpu` is true and Metal is available, Phase 1 runs on GPU. Falls back
/// to the Rayon CPU path on any GPU error. No effect on non-macOS builds.
pub fn fsm_classify_two_step(
    states: usize,
    actions: usize,
    sample: usize,
    trace_steps: usize,
    seed: u64,
    full_states_only: bool,
    use_gpu: bool,
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
        let mut rng_state = 0x5DEECE66Du64.wrapping_mul(seed).wrapping_add(0xBu64);
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
    let is_exhaustive = sample == 0;

    // Step 1: Canonicalize each FSM index -> structural key, keep min-index per key.
    // If full_states_only is set, skip FSMs that don't reach all states from the
    // start state (matching WL's VertexCount[FSMGraph[#]] == s filter).
    //
    // The structural key is hashed to u128 so the map stays compact (16 B per key)
    // and matches the GPU path's output format. Collision probability is < 10^-20
    // at 10^9 entries.
    //
    // GPU path is only used for exhaustive enumeration of the full index space —
    // samples stay on CPU since the sample set might not be a contiguous range.
    let canonical_by_key: HashMap<u128, u64> = if use_gpu && is_exhaustive {
        match crate::gpu::run_fsm_canonicalize_gpu(
            total_rules as u64,
            states as u32,
            actions as u32,
            full_states_only,
        ) {
            Ok(m) => m,
            Err(e) => {
                eprintln!(
                    "[fsm_classify] GPU Phase 1 unavailable ({}); falling back to CPU.",
                    e
                );
                canonicalize_cpu(&candidates, states, actions, full_states_only)?
            }
        }
    } else {
        canonicalize_cpu(&candidates, states, actions, full_states_only)?
    };

    let canonical_count = canonical_by_key.len();
    let mut canonical_reps: Vec<u64> = canonical_by_key.into_values().collect();
    canonical_reps.sort_unstable();

    // Step 2: For each canonical representative, compute behavioral trace -> group by behavior.
    //
    // Key is a 128-bit splitmix-based hash of the depth-12 trace. The CPU and
    // GPU paths produce byte-identical hashes (same splitmix64 primitive), so
    // behavior groups match either way. Birthday collision at 10^9 entries is
    // < 10^-20, negligible for problem sizes we care about.
    let behavior_by_key: HashMap<u128, Vec<u64>> = if use_gpu {
        match crate::gpu::run_fsm_trace_hash_gpu(
            &canonical_reps,
            states as u32,
            actions as u32,
            trace_steps as u32,
        ) {
            Ok(hashes) => {
                let mut map: HashMap<u128, Vec<u64>> =
                    HashMap::with_capacity(canonical_reps.len());
                for (&idx, &key) in canonical_reps.iter().zip(hashes.iter()) {
                    map.entry(key).or_default().push(idx);
                }
                map
            }
            Err(e) => {
                eprintln!(
                    "[fsm_classify] GPU Phase 2 unavailable ({}); falling back to CPU.",
                    e
                );
                trace_hash_cpu(&canonical_reps, states, actions, trace_steps)?
            }
        }
    } else {
        trace_hash_cpu(&canonical_reps, states, actions, trace_steps)?
    };

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
    let mut prev_a: u8 = 0;
    let mut prev_b: u8 = 0;

    for round in 0..rounds {
        let move_a = if runner_a.is_fsm() {
            runner_a.get_fsm_move(prev_b, round)
        } else {
            runner_a.get_move(&history, round)
        };
        let move_a = match move_a {
            Some(m) => m,
            None => return (None, 1),
        };
        let move_b = if runner_b.is_fsm() {
            runner_b.get_fsm_move(prev_a, round)
        } else {
            runner_b.get_move(&history, round)
        };
        let move_b = match move_b {
            Some(m) => m,
            None => return (None, 2),
        };

        history.push(move_a);
        history.push(move_b);
        prev_a = move_a;
        prev_b = move_b;

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
    let mut prev_a: u8 = 0;
    let mut prev_b: u8 = 0;

    for round in 0..rounds {
        // For FSMs, use get_fsm_move with the correct opponent's last action.
        // The generic get_move reads history.last() which is always B's move,
        // giving player B its own move instead of the opponent's.
        let move_a = if runner_a.is_fsm() {
            runner_a.get_fsm_move(prev_b, round)
        } else {
            runner_a.get_move(&history, round)
        };
        let move_a = match move_a {
            Some(m) => m,
            None => return (None, 1),
        };
        let move_b = if runner_b.is_fsm() {
            runner_b.get_fsm_move(prev_a, round)
        } else {
            runner_b.get_move(&history, round)
        };
        let move_b = match move_b {
            Some(m) => m,
            None => return (None, 2),
        };

        history.push(move_a);
        history.push(move_b);
        prev_a = move_a;
        prev_b = move_b;

        let idx = (move_a as usize) * payoff.num_actions + move_b as usize;
        if let Some(payoffs) = payoff.get(idx) {
            score_a += payoffs[0] as i64;
            score_b += payoffs[1] as i64;
        }
    }

    (Some((score_a, score_b)), 0)
}

/// Play an iterated game between two strategy runners, returning the full move history.
/// `initial_history` seeds the game: FSM states are advanced through it, and it is
/// prepended to the returned history. `rounds` additional rounds are then played.
/// Returns (history, failed_flag) where history is a Vec of [move_a, move_b] pairs.
/// failed_flag: 0=none, 1=a failed, 2=b failed, 3=both failed.
pub fn play_game_with_history(
    runner_a: &mut StrategyRunner,
    runner_b: &mut StrategyRunner,
    rounds: u32,
    initial_history: &[[u8; 2]],
) -> (Vec<[u8; 2]>, u8) {
    runner_a.reset();
    runner_b.reset();

    let init_len = initial_history.len() as u32;
    let total = init_len + rounds;
    let mut flat_history: Vec<u8> = Vec::with_capacity(2 * total as usize);
    let mut move_history: Vec<[u8; 2]> = Vec::with_capacity(total as usize);
    let mut prev_a: u8 = 0;
    let mut prev_b: u8 = 0;

    // Replay initial history: advance FSM states, populate flat history
    for (round, &[ma, mb]) in initial_history.iter().enumerate() {
        if runner_a.is_fsm() {
            runner_a.get_fsm_move(prev_b, round as u32);
        }
        if runner_b.is_fsm() {
            runner_b.get_fsm_move(prev_a, round as u32);
        }
        flat_history.push(ma);
        flat_history.push(mb);
        move_history.push([ma, mb]);
        prev_a = ma;
        prev_b = mb;
    }

    // Play new rounds (abort on non-halt for tournament safety)
    for round in init_len..total {
        let move_a = if runner_a.is_fsm() {
            runner_a.get_fsm_move(prev_b, round)
        } else {
            runner_a.get_move(&flat_history, round)
        };
        let move_a = match move_a {
            Some(m) => m,
            None => return (move_history, 1),
        };
        let move_b = if runner_b.is_fsm() {
            runner_b.get_fsm_move(prev_a, round)
        } else {
            runner_b.get_move(&flat_history, round)
        };
        let move_b = match move_b {
            Some(m) => m,
            None => return (move_history, 2),
        };

        flat_history.push(move_a);
        flat_history.push(move_b);
        move_history.push([move_a, move_b]);
        prev_a = move_a;
        prev_b = move_b;
    }

    (move_history, 0)
}

/// Like `play_game_with_history` but continues on non-halt using sentinel 255.
/// Used by ProgramIteratedGame to show Undefined moves instead of aborting.
pub fn play_game_with_history_sentinel(
    runner_a: &mut StrategyRunner,
    runner_b: &mut StrategyRunner,
    rounds: u32,
    initial_history: &[[u8; 2]],
) -> (Vec<[u8; 2]>, u8) {
    runner_a.reset();
    runner_b.reset();

    let init_len = initial_history.len() as u32;
    let total = init_len + rounds;
    let mut flat_history: Vec<u8> = Vec::with_capacity(2 * total as usize);
    let mut move_history: Vec<[u8; 2]> = Vec::with_capacity(total as usize);
    let mut prev_a: u8 = 0;
    let mut prev_b: u8 = 0;

    for (round, &[ma, mb]) in initial_history.iter().enumerate() {
        if runner_a.is_fsm() { runner_a.get_fsm_move(prev_b, round as u32); }
        if runner_b.is_fsm() { runner_b.get_fsm_move(prev_a, round as u32); }
        flat_history.push(ma);
        flat_history.push(mb);
        move_history.push([ma, mb]);
        prev_a = ma;
        prev_b = mb;
    }

    let mut failed: u8 = 0;
    for round in init_len..total {
        let move_a = if runner_a.is_fsm() {
            runner_a.get_fsm_move(prev_b, round)
        } else {
            runner_a.get_move(&flat_history, round)
        };
        let move_a = match move_a {
            Some(m) => m,
            None => { failed |= 1; 255 },
        };
        let move_b = if runner_b.is_fsm() {
            runner_b.get_fsm_move(prev_a, round)
        } else {
            runner_b.get_move(&flat_history, round)
        };
        let move_b = match move_b {
            Some(m) => m,
            None => { failed |= 2; 255 },
        };

        flat_history.push(move_a);
        flat_history.push(move_b);
        move_history.push([move_a, move_b]);
        prev_a = move_a;
        prev_b = move_b;
    }

    (move_history, failed)
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

    // ── decode_fsm_from_str: bigint-safe decoding ────────────────────────

    #[test]
    fn fsm_decode_from_str_matches_u64_for_small_space() {
        for id in 0..64u64 {
            let via_u64 = decode_fsm(id, 2, 2);
            let via_str = decode_fsm_from_str(&id.to_string(), 2, 2);
            assert_eq!(via_str, via_u64, "id {} mismatch", id);
        }
    }

    #[test]
    fn fsm_decode_from_str_fast_path_at_u64_max() {
        // u64::MAX itself parses as u64, so both paths go through decode_fsm.
        let id_str = u64::MAX.to_string();
        let via_str = decode_fsm_from_str(&id_str, 9, 2);
        let via_u64 = decode_fsm(u64::MAX, 9, 2);
        assert_eq!(via_str, via_u64);
    }

    #[test]
    fn fsm_decode_from_str_big_id_is_not_fsm_zero() {
        // 49513043775795713316 > u64::MAX. The previous
        // `id.parse::<u64>().unwrap_or(0)` path silently decoded this as FSM #0.
        let big = "49513043775795713316";
        assert!(big.parse::<u64>().is_err(), "precondition: overflows u64");
        let big_decoded = decode_fsm_from_str(big, 9, 2);
        let zero_decoded = decode_fsm(0, 9, 2);
        assert_ne!(big_decoded, zero_decoded,
            "big id must not collapse to FSM #0 (old bug)");
    }

    #[test]
    fn fsm_decode_from_str_big_id_respects_ranges() {
        for id in &[
            "49513043775795713316",
            "20000000000000000000",
            "76848453272063549951",
        ] {
            let (outputs, transitions) = decode_fsm_from_str(id, 9, 2);
            assert_eq!(outputs.len(), 9);
            assert_eq!(transitions.len(), 18);
            for &o in &outputs {
                assert!(o < 2, "output out of range for id {}: {}", id, o);
            }
            for &t in &transitions {
                assert!(t < 9, "transition out of range for id {}: {}", id, t);
            }
        }
    }

    #[test]
    fn fsm_decode_from_str_matches_u128_reference_above_u64() {
        // For index > u64::MAX, the encoding is just (idx-1) = tc*k^s + oc with
        // positive idx-1 — no signed-abs edge case. A plain u128 implementation
        // is a trustworthy reference in that regime.
        fn reference_u128(index: u128, states: usize, actions: usize) -> (Vec<u8>, Vec<usize>) {
            let n = states * actions;
            let action_block = (actions as u128).pow(states as u32);
            let mut tc = (index - 1) / action_block;
            let mut oc = (index - 1) % action_block;

            let mut next_digits = vec![0usize; n];
            for i in (0..n).rev() {
                next_digits[i] = (tc % states as u128) as usize;
                tc /= states as u128;
            }
            let mut output_digits = vec![0usize; states];
            for i in (0..states).rev() {
                output_digits[i] = (oc % actions as u128) as usize;
                oc /= actions as u128;
            }

            let outputs: Vec<u8> = output_digits.into_iter().map(|d| d as u8).collect();
            let mut transitions = vec![0usize; n];
            for state_idx in 0..states {
                for input_idx in 0..actions {
                    let flat_idx = state_idx * actions + input_idx;
                    transitions[flat_idx] = next_digits[flat_idx].min(states - 1);
                }
            }
            (outputs, transitions)
        }

        let u64_max = u64::MAX as u128;
        for offset in [1u128, 2, 100, 1_000_000, 49_513_043_775_795_713_316u128 - u64_max] {
            let index = u64_max + offset;
            let id_str = index.to_string();
            let via_str = decode_fsm_from_str(&id_str, 9, 2);
            let via_ref = reference_u128(index, 9, 2);
            assert_eq!(via_str, via_ref, "mismatch for id {}", id_str);
        }
    }

    #[test]
    fn fsm_runner_big_id_does_not_behave_as_fsm_zero() {
        // End-to-end: StrategyRunner with a big-id FSM must diverge from
        // StrategyRunner with FSM #0 during play. Regression guard for the
        // IteratedGame overflow bug.
        let spec_big = StrategySpec::Fsm {
            id: "49513043775795713316".to_string(),
            s: 9, k: 2, num_actions: 2,
        };
        let spec_zero = StrategySpec::Fsm {
            id: "0".to_string(),
            s: 9, k: 2, num_actions: 2,
        };
        let mut big = StrategyRunner::new(&spec_big);
        let mut zero = StrategyRunner::new(&spec_zero);
        let mut opp_big = StrategyRunner::new(&StrategySpec::Fsm {
            id: "0".to_string(), s: 1, k: 2, num_actions: 2,
        });
        let mut opp_zero = StrategyRunner::new(&StrategySpec::Fsm {
            id: "0".to_string(), s: 1, k: 2, num_actions: 2,
        });
        let (hist_big, _) = play_game_with_history_sentinel(&mut big, &mut opp_big, 20, &[]);
        let (hist_zero, _) = play_game_with_history_sentinel(&mut zero, &mut opp_zero, 20, &[]);
        assert_ne!(hist_big, hist_zero, "big-id FSM must not behave as FSM #0");
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
        let spec = StrategySpec::Fsm { id: "0".to_string(), s: 2, k: 2, num_actions: 2 };
        let mut runner = StrategyRunner::new(&spec);
        let m = runner.get_move(&[], 0).unwrap();
        // FSM outputs based on initial state; the exact value depends on the FSM
        assert!(m < 2);
    }

    #[test]
    fn fsm_runner_stateful_across_rounds() {
        // Create an FSM and verify it can change state
        let spec = StrategySpec::Fsm { id: "47".to_string(), s: 2, k: 2, num_actions: 2 };
        let mut runner = StrategyRunner::new(&spec);
        let m0 = runner.get_move(&[], 0).unwrap();
        let m1 = runner.get_move(&[m0, 1], 1).unwrap();
        assert!(m0 < 2);
        assert!(m1 < 2);
    }

    #[test]
    fn fsm_runner_reset_restores_state() {
        let spec = StrategySpec::Fsm { id: "47".to_string(), s: 2, k: 2, num_actions: 2 };
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
        // Elementary CA: k=2, r=1 -> two_r=2, neighborhood=3
        // WL convention: table[i] = Floor[30/2^i] mod 2 (LSD indexing)
        // table = [0, 1, 1, 1, 1, 0, 0, 0]
        let table = decode_ca_rule_table(30, 2, 2);
        assert_eq!(table.len(), 8); // 2^3 = 8
        assert_eq!(table, vec![0, 1, 1, 1, 1, 0, 0, 0]);
    }

    #[test]
    fn ca_rule_table_elementary_rule_110() {
        // WL convention: table[i] = Floor[110/2^i] mod 2 (LSD indexing)
        // table = [0, 1, 1, 1, 0, 1, 1, 0]
        let table = decode_ca_rule_table(110, 2, 2);
        assert_eq!(table.len(), 8);
        assert_eq!(table, vec![0, 1, 1, 1, 0, 1, 1, 0]);
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
            rule: "110".to_string(),
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
            rule: "110".to_string(),
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

    // ── CA WL-compatibility tests ──────────────────────────────────────

    /// Verify decode_ca_rule_table + MSD lookup matches WL CellularAutomatonToRule.
    /// WL: Thread[Reverse[Tuples[Range[0,k-1], 2r+1]] -> IntegerDigits[n, k, k^(2r+1)]]
    /// For k=2, r=1/2: neighborhood {w0,w1}, output = Floor[rule / 2^(w0*2+w1)] mod 2.
    #[test]
    fn ca_rule_table_matches_wl_k2_r_half() {
        // k=2, r=1/2, table_len=4
        for rule in 0u64..16 {
            let table = decode_ca_rule_table(rule, 2, 1);
            for w0 in 0u8..2 {
                for w1 in 0u8..2 {
                    let msd_idx = (w0 as usize) * 2 + w1 as usize;
                    let rust_out = table[msd_idx];
                    // WL: output = Floor[rule / 2^(w0*2+w1)] mod 2
                    let wl_out = ((rule >> msd_idx) & 1) as u8;
                    assert_eq!(
                        rust_out, wl_out,
                        "rule {}, neighborhood ({},{}): Rust {} != WL {}",
                        rule, w0, w1, rust_out, wl_out
                    );
                }
            }
        }
    }

    #[test]
    fn ca_rule_table_matches_wl_k2_r1() {
        // k=2, r=1, table_len=8
        for rule in 0u64..256 {
            let table = decode_ca_rule_table(rule, 2, 2);
            for w0 in 0u8..2 {
                for w1 in 0u8..2 {
                    for w2 in 0u8..2 {
                        let msd_idx = (w0 as usize) * 4 + (w1 as usize) * 2 + w2 as usize;
                        let rust_out = table[msd_idx];
                        let wl_out = ((rule >> msd_idx) & 1) as u8;
                        assert_eq!(
                            rust_out, wl_out,
                            "rule {}, neighborhood ({},{},{}): Rust {} != WL {}",
                            rule, w0, w1, w2, rust_out, wl_out
                        );
                    }
                }
            }
        }
    }

    /// Verify ShrinkingCA single step matches WL for specific cases.
    /// WL: ShrinkingCA[{rule, 2, 1/2}][{0,0,1,1}] should give specific output.
    #[test]
    fn shrinking_ca_step_matches_wl_rule1() {
        // Rule 1, k=2, r=1/2: table[msd_idx] = Floor[1/2^msd_idx] mod 2
        // table = [1, 0, 0, 0] (only {0,0}->1)
        let table = decode_ca_rule_table(1, 2, 1);
        assert_eq!(table, vec![1, 0, 0, 0]);

        // Apply to row [0, 0, 1, 1]:
        // Windows: {0,0}->1, {0,1}->0, {1,1}->0
        // Next row: [1, 0, 0]
        let rows = run_shrinking_ca(&table, 2, 1, 1, &[0, 0, 1, 1]);
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[1], vec![1, 0, 0]);
    }

    #[test]
    fn shrinking_ca_step_matches_wl_rule11() {
        // Rule 11 = 1011 binary
        // table[0]={0,0}->1, table[1]={0,1}->1, table[2]={1,0}->0, table[3]={1,1}->1
        let table = decode_ca_rule_table(11, 2, 1);
        assert_eq!(table, vec![1, 1, 0, 1]);

        // Apply to [0, 1, 0, 1]:
        // Windows: {0,1}->1, {1,0}->0, {0,1}->1
        let rows = run_shrinking_ca(&table, 2, 1, 1, &[0, 1, 0, 1]);
        assert_eq!(rows[1], vec![1, 0, 1]);
    }

    // ── TM runner tests ──────────────────────────────────────────────────

    #[test]
    fn tm_runner_cooperates_round_0() {
        let spec = StrategySpec::Tm {
            id: "64".to_string(),
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
            id: "64".to_string(),
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
            id: "64".to_string(),
            s: 2,
            k: 2,
            max_steps: 500,
            num_actions: 2,
        };
        let spec_b = StrategySpec::Tm {
            id: "64".to_string(),
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
        let spec_a = StrategySpec::Fsm { id: "1".to_string(), s: 1, k: 2, num_actions: 2 };
        let spec_b = StrategySpec::Fsm { id: "0".to_string(), s: 1, k: 2, num_actions: 2 };
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
            id: "64".to_string(),
            s: 2,
            k: 2,
            max_steps: 500,
            num_actions: 2,
        };
        let spec_b = StrategySpec::Fsm { id: "0".to_string(), s: 1, k: 2, num_actions: 2 };
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
                assert_eq!(id, "323");
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
                assert_eq!(id, "47");
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
                assert_eq!(rule, "110");
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

    // ── FSM classify: full-states-only filter matches WL $FSMUnique ──────
    //
    // Mirrors the WL pipeline used in new Code-02.nb:
    //   tups = Tuples[{1, 0}, 12];
    //   Select[CanonicalFiniteStateMachineIndices[s, k],
    //     VertexCount[FSMGraph[{#, s, k}]] == s &]  (* drop FSMs w/ dead states *)
    //   GroupBy[..., Last -> First]
    //   $FSMUnique = First /@ groups
    //
    // Rust equivalent: fsm_classify_two_step(s, k, 0, 12, _, full_states_only=true)
    // then take `representatives`.

    #[test]
    fn fsm_unique_22_matches_wl() {
        let expected: Vec<u64> = vec![
            17, 18, 19, 20, 22, 23, 26, 27, 30, 31, 34, 35, 38, 39, 46, 47, 50, 51,
            54, 55, 58, 59,
        ];
        let cls = fsm_classify_two_step(2, 2, 0, 12, 42, true, false).unwrap();
        assert_eq!(
            cls.representatives, expected,
            "FSM (2,2) classification with full_states_only should match WL $FSMUnique22"
        );
    }

    // s=4 exhaustive classification count (matches cached $FSMUnique42 from
    // new Code-02.nb, length 51924). ~3s, small memory — fine to run by default.
    #[test]
    fn fsm_unique_42_count_matches_wl() {
        let cls = fsm_classify_two_step(4, 2, 0, 12, 42, true, false).unwrap();
        assert_eq!(cls.canonical_count, 83968, "canonical count for (4,2)");
        assert_eq!(
            cls.unique_behaviors, 51924,
            "unique count for (4,2) should match WL $FSMUnique42"
        );
    }

    // s=5 exhaustive: ~3 min on ~12 cores, ~6 GB peak RSS. Run with:
    //   cargo test --release --no-default-features fsm_unique_52 -- --ignored --nocapture
    #[test]
    #[ignore]
    fn fsm_unique_52_count() {
        let cls = fsm_classify_two_step(5, 2, 0, 12, 42, true, false).unwrap();
        assert_eq!(cls.canonical_count, 5_141_600, "canonical count for (5,2)");
        assert_eq!(
            cls.unique_behaviors, 3_415_990,
            "unique count for (5,2) — first exhaustive ground truth"
        );
    }

    // GPU parity: same representatives as CPU for small cases. Only runs on
    // macOS with the `metal` feature; skipped elsewhere.
    #[test]
    #[cfg(all(target_os = "macos", feature = "metal"))]
    fn fsm_unique_22_matches_wl_gpu() {
        let expected: Vec<u64> = vec![
            17, 18, 19, 20, 22, 23, 26, 27, 30, 31, 34, 35, 38, 39, 46, 47, 50, 51,
            54, 55, 58, 59,
        ];
        let cls = fsm_classify_two_step(2, 2, 0, 12, 42, true, true).unwrap();
        assert_eq!(cls.representatives, expected,
            "GPU (2,2) representatives must match WL $FSMUnique22");
    }

    #[test]
    #[cfg(all(target_os = "macos", feature = "metal"))]
    fn fsm_unique_32_count_matches_cpu_gpu() {
        let cpu = fsm_classify_two_step(3, 2, 0, 12, 42, true, false).unwrap();
        let gpu = fsm_classify_two_step(3, 2, 0, 12, 42, true, true).unwrap();
        assert_eq!(gpu.canonical_count, cpu.canonical_count);
        assert_eq!(gpu.unique_behaviors, cpu.unique_behaviors);
        assert_eq!(gpu.representatives, cpu.representatives);
    }

    #[test]
    #[cfg(all(target_os = "macos", feature = "metal"))]
    fn fsm_unique_42_count_matches_cpu_gpu() {
        let cpu = fsm_classify_two_step(4, 2, 0, 12, 42, true, false).unwrap();
        let gpu = fsm_classify_two_step(4, 2, 0, 12, 42, true, true).unwrap();
        assert_eq!(gpu.canonical_count, cpu.canonical_count,
            "GPU canonical count (4,2) must equal CPU: {} vs {}",
            gpu.canonical_count, cpu.canonical_count);
        assert_eq!(gpu.unique_behaviors, 51924);
        assert_eq!(gpu.representatives, cpu.representatives,
            "GPU representatives (4,2) must match CPU byte-for-byte");
    }

    #[test]
    fn fsm_unique_32_matches_wl() {
        // Full 956-element list from new Code-02.nb's $FSMUnique32.
        let expected: Vec<u64> = vec![
            793, 794, 795, 796, 797, 798, 799, 800, 802, 803, 804, 805, 806, 807,
            810, 811, 814, 815, 818, 819, 820, 821, 822, 823, 826, 827, 828, 829,
            830, 831, 834, 835, 836, 837, 838, 839, 842, 843, 844, 845, 846, 847,
            850, 852, 853, 855, 858, 859, 860, 861, 862, 863, 1010, 1011, 1012,
            1013, 1014, 1015, 1018, 1019, 1020, 1021, 1022, 1023, 1026, 1027, 1028,
            1029, 1030, 1031, 1034, 1035, 1036, 1037, 1038, 1039, 1042, 1043, 1044,
            1045, 1046, 1047, 1050, 1051, 1054, 1055, 1058, 1059, 1060, 1061, 1062,
            1063, 1066, 1071, 1074, 1075, 1078, 1079, 1082, 1083, 1084, 1085, 1086,
            1087, 1090, 1092, 1093, 1095, 1098, 1099, 1100, 1101, 1102, 1103, 1106,
            1107, 1110, 1111, 1114, 1115, 1116, 1117, 1118, 1119, 1122, 1123, 1124,
            1125, 1126, 1127, 1130, 1131, 1134, 1135, 1138, 1140, 1141, 1143, 1146,
            1147, 1148, 1149, 1150, 1151, 1154, 1155, 1156, 1157, 1158, 1159, 1162,
            1164, 1165, 1167, 1170, 1171, 1172, 1173, 1174, 1175, 1178, 1179, 1180,
            1181, 1182, 1183, 1186, 1187, 1190, 1191, 1194, 1195, 1198, 1199, 1202,
            1203, 1204, 1205, 1206, 1207, 1210, 1215, 1218, 1219, 1222, 1223, 1226,
            1227, 1228, 1229, 1230, 1231, 1234, 1236, 1237, 1239, 1242, 1243, 1244,
            1245, 1246, 1247, 1250, 1251, 1252, 1253, 1254, 1255, 1258, 1259, 1262,
            1263, 1266, 1267, 1270, 1271, 1274, 1275, 1276, 1277, 1278, 1279, 1282,
            1287, 1290, 1291, 1294, 1295, 2090, 2091, 2092, 2093, 2094, 2095, 2098,
            2099, 2100, 2101, 2102, 2103, 2106, 2107, 2110, 2111, 2114, 2115, 2116,
            2117, 2118, 2119, 2122, 2123, 2124, 2125, 2126, 2127, 2130, 2132, 2133,
            2135, 2138, 2139, 2140, 2141, 2142, 2143, 2146, 2147, 2148, 2149, 2150,
            2151, 2154, 2155, 2156, 2157, 2158, 2159, 2306, 2307, 2308, 2309, 2310,
            2311, 2314, 2315, 2316, 2317, 2318, 2319, 2322, 2323, 2324, 2325, 2326,
            2327, 2330, 2332, 2333, 2335, 2338, 2339, 2340, 2341, 2342, 2343, 2346,
            2351, 2354, 2355, 2356, 2357, 2358, 2359, 2362, 2363, 2366, 2367, 2370,
            2371, 2374, 2375, 2378, 2379, 2380, 2381, 2382, 2383, 2386, 2387, 2388,
            2389, 2390, 2391, 2394, 2395, 2396, 2397, 2398, 2399, 2402, 2407, 2410,
            2411, 2412, 2413, 2414, 2415, 2418, 2420, 2421, 2423, 2426, 2427, 2430,
            2431, 2434, 2435, 2436, 2437, 2438, 2439, 2442, 2443, 2444, 2445, 2446,
            2447, 2450, 2451, 2452, 2453, 2454, 2455, 2458, 2459, 2460, 2461, 2462,
            2463, 2466, 2467, 2468, 2469, 2470, 2471, 2474, 2476, 2477, 2479, 2482,
            2483, 2486, 2487, 2490, 2495, 2498, 2499, 2500, 2501, 2502, 2503, 2506,
            2507, 2510, 2511, 2514, 2515, 2518, 2519, 2522, 2523, 2524, 2525, 2526,
            2527, 2530, 2531, 2532, 2533, 2534, 2535, 2538, 2539, 2540, 2541, 2542,
            2543, 2546, 2548, 2549, 2551, 2554, 2555, 2558, 2559, 2562, 2567, 2570,
            2571, 2572, 2573, 2574, 2575, 2578, 2579, 2582, 2583, 2586, 2587, 2590,
            2591, 2738, 2739, 2740, 2741, 2742, 2743, 2746, 2747, 2748, 2749, 2750,
            2751, 2754, 2755, 2758, 2759, 2762, 2763, 2764, 2765, 2766, 2767, 2770,
            2771, 2772, 2773, 2774, 2775, 2778, 2779, 2780, 2781, 2782, 2783, 2786,
            2787, 2788, 2789, 2790, 2791, 2794, 2795, 2796, 2797, 2798, 2799, 2802,
            2803, 2804, 2805, 2806, 2807, 2954, 2955, 2956, 2957, 2958, 2959, 2962,
            2963, 2964, 2965, 2966, 2967, 2970, 2971, 2972, 2973, 2974, 2975, 2978,
            2979, 2980, 2981, 2982, 2983, 2986, 2987, 2994, 2995, 3002, 3003, 3004,
            3005, 3006, 3007, 3010, 3011, 3018, 3019, 3026, 3027, 3028, 3029, 3030,
            3031, 3034, 3035, 3036, 3037, 3038, 3039, 3042, 3043, 3044, 3045, 3046,
            3047, 3050, 3051, 3054, 3055, 3066, 3067, 3068, 3069, 3070, 3071, 3074,
            3075, 3078, 3079, 3082, 3083, 3084, 3085, 3086, 3087, 3090, 3091, 3092,
            3093, 3094, 3095, 3098, 3099, 3100, 3101, 3102, 3103, 3106, 3107, 3108,
            3109, 3110, 3111, 3114, 3115, 3116, 3117, 3118, 3119, 3122, 3123, 3124,
            3125, 3126, 3127, 3138, 3139, 3146, 3147, 3148, 3149, 3150, 3151, 3154,
            3155, 3162, 3163, 3170, 3171, 3172, 3173, 3174, 3175, 3178, 3179, 3180,
            3181, 3182, 3183, 3186, 3187, 3188, 3189, 3190, 3191, 3194, 3195, 3196,
            3197, 3198, 3199, 3210, 3211, 3218, 3219, 3220, 3221, 3222, 3223, 3226,
            3227, 3234, 3235, 3242, 3243, 3246, 3247, 3250, 3251, 3252, 3253, 3254,
            3255, 3258, 3259, 3260, 3261, 3262, 3263, 3266, 3268, 3269, 3271, 3274,
            3275, 3276, 3277, 3278, 3279, 3290, 3291, 3292, 3293, 3294, 3295, 3298,
            3299, 3300, 3301, 3302, 3303, 3306, 3307, 3308, 3309, 3310, 3311, 3314,
            3315, 3316, 3317, 3318, 3319, 3322, 3323, 3326, 3327, 3330, 3331, 3334,
            3335, 3338, 3340, 3341, 3343, 3346, 3347, 3348, 3349, 3350, 3351, 3362,
            3363, 3364, 3365, 3366, 3367, 3370, 3371, 3372, 3373, 3374, 3375, 3378,
            3379, 3380, 3381, 3382, 3383, 3387, 3388, 3389, 3390, 3395, 3398, 3403,
            3406, 3412, 3413, 3419, 3420, 3421, 3422, 3435, 3436, 3437, 3438, 3443,
            3444, 3445, 3446, 3451, 3452, 3453, 3454, 3458, 3459, 3460, 3461, 3462,
            3463, 3466, 3467, 3468, 3469, 3470, 3471, 3474, 3475, 3476, 3477, 3478,
            3479, 3482, 3487, 3490, 3491, 3492, 3493, 3494, 3495, 3506, 3507, 3510,
            3511, 3514, 3515, 3516, 3517, 3518, 3519, 3522, 3523, 3524, 3525, 3526,
            3527, 3530, 3531, 3532, 3533, 3534, 3535, 3538, 3539, 3540, 3541, 3542,
            3543, 3546, 3547, 3548, 3549, 3550, 3551, 3554, 3556, 3557, 3559, 3562,
            3563, 3578, 3579, 3580, 3581, 3582, 3583, 3586, 3587, 3594, 3595, 3674,
            3675, 3676, 3677, 3678, 3679, 3682, 3683, 3684, 3685, 3686, 3687, 3690,
            3691, 3692, 3693, 3694, 3695, 3698, 3703, 3706, 3707, 3708, 3709, 3710,
            3711, 3722, 3723, 3726, 3727, 3730, 3731, 3732, 3733, 3734, 3735, 3738,
            3739, 3740, 3741, 3742, 3743, 3746, 3747, 3748, 3749, 3750, 3751, 3754,
            3755, 3756, 3757, 3758, 3759, 3762, 3763, 3764, 3765, 3766, 3767, 3770,
            3772, 3773, 3775, 3778, 3779, 3794, 3795, 3796, 3797, 3798, 3799, 3802,
            3803, 3810, 3811, 3818, 3819, 3820, 3821, 3822, 3823, 3826, 3827, 3828,
            3829, 3830, 3831, 3834, 3835, 3836, 3837, 3838, 3839, 3842, 3844, 3845,
            3847, 3850, 3851, 3866, 3867, 3868, 3869, 3870, 3871, 3874, 3875, 3882,
            3883,
        ];
        assert_eq!(expected.len(), 956, "expected list length sanity");

        let cls = fsm_classify_two_step(3, 2, 0, 12, 42, true, false).unwrap();
        assert_eq!(
            cls.representatives.len(),
            expected.len(),
            "FSM (3,2) representative count mismatch"
        );
        assert_eq!(
            cls.representatives, expected,
            "FSM (3,2) classification with full_states_only should match WL $FSMUnique32"
        );
    }
}
