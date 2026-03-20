//! Metal GPU accelerator for TM halting search and tournament.
//!
//! Each GPU thread tests one (TM, input) pair: decodes input to tape,
//! runs the TM for up to max_steps, reports halted flag.
//! CPU dispatches per-depth batches and aggregates results.
//!
//! Tournament kernel: each GPU thread plays one pair of TMs for all rounds.

#[cfg(all(target_os = "macos", feature = "metal"))]
mod metal_impl {
    use metal::*;
    use std::ffi::c_void;
    use std::mem::size_of;

    /// Parameters passed to the GPU kernel via constant buffer.
    #[repr(C)]
    #[derive(Copy, Clone)]
    struct GpuParams {
        num_tms: u32,
        states: u32,
        symbols: u32,
        max_steps: u32,
        num_inputs: u32,
        input_width: u32, // number of digits for this depth
    }

    /// One transition entry, padded for GPU alignment.
    #[repr(C)]
    #[derive(Copy, Clone)]
    struct GpuTransition {
        write: u32,
        move_right: u32, // 0=Left, 1=Right
        next: u32,       // 1-indexed
        _pad: u32,
    }

    /// Parameters for the tournament GPU kernel.
    #[repr(C)]
    #[derive(Copy, Clone)]
    struct GameParams {
        num_pairs: u32,
        states: u32,
        symbols: u32,
        max_steps: u32,
        rounds: u32,
        num_actions: u32,
        _pad: [u32; 2],
    }

    const SHADER_SOURCE: &str = r#"
#include <metal_stdlib>
using namespace metal;

struct GpuParams {
    uint num_tms;
    uint states;
    uint symbols;
    uint max_steps;
    uint num_inputs;
    uint input_width;
};

struct GpuTransition {
    uint write;
    uint move_right;
    uint next;
    uint _pad;
};

// Max tape: max_steps left extensions + input digits.
// Apple Silicon supports ~16KB per-thread stack; 11000 bytes is safe.
#define MAX_TAPE 11000

kernel void tm_halting_test(
    device const GpuTransition* all_transitions [[buffer(0)]],
    device const uint* inputs             [[buffer(1)]],
    device atomic_uint* halted_flags      [[buffer(2)]],
    constant GpuParams& params            [[buffer(3)]],
    device const uint* step_limits        [[buffer(4)]],
    uint gid [[thread_position_in_grid]])
{
    // gid encodes (tm_idx * num_inputs + input_idx)
    uint tm_idx = gid / params.num_inputs;
    uint input_idx = gid % params.num_inputs;

    if (tm_idx >= params.num_tms) return;

    // Early exit: another thread already proved this TM non-halting.
    if (atomic_load_explicit(&halted_flags[tm_idx], memory_order_relaxed) != 0u) return;

    // Per-TM step limit: use the TM's known worst_steps * 2 + margin,
    // capped at global max_steps. This avoids 10K iterations for TMs
    // that always halt in 20 steps.
    uint my_max_steps = step_limits[tm_idx];

    uint input_val = inputs[input_idx];
    uint k = params.symbols;
    uint trans_per_tm = params.states * k;
    device const GpuTransition* trans = all_transitions + tm_idx * trans_per_tm;

    // Decode input_val into digits (MSD-first)
    uint input_width = params.input_width;
    uchar tape[MAX_TAPE];
    uint left_pad = min(my_max_steps, (uint)(MAX_TAPE - input_width - 1));
    uint tape_right = left_pad + input_width;

    // Zero tape
    for (uint i = 0; i < tape_right; i++) {
        tape[i] = 0;
    }

    // Write input digits (binary fast path)
    if (k == 2) {
        for (uint i = 0; i < input_width; i++) {
            tape[left_pad + i] = (input_val >> (input_width - 1 - i)) & 1u;
        }
    } else {
        uint tmp = input_val;
        for (uint i = input_width; i > 0; i--) {
            tape[left_pad + i - 1] = tmp % k;
            tmp /= k;
        }
    }

    // Run TM
    uint head = tape_right - 1;
    uint state = 1; // start state (1-indexed)
    bool did_halt = false;

    for (uint step = 0; step < my_max_steps; step++) {
        uchar read_sym = tape[head];
        uint idx = (state - 1) * k + read_sym;
        GpuTransition t = trans[idx];

        tape[head] = (uchar)t.write;

        if (t.move_right != 0u) {
            if (head + 1 == tape_right) {
                did_halt = true;
                break;
            }
            head++;
        } else {
            if (head == 0) {
                // Ran out of left padding → treat as non-halting
                break;
            }
            head--;
        }

        state = t.next;
        if (state == 0u) break;
    }

    if (!did_halt) {
        atomic_fetch_or_explicit(&halted_flags[tm_idx], 1u, memory_order_relaxed);
    }
}
"#;

    const TOURNAMENT_SHADER_SOURCE: &str = r#"
#include <metal_stdlib>
using namespace metal;

struct GpuTransition {
    uint write;
    uint move_right;
    uint next;
    uint _pad;
};

struct GameParams {
    uint num_pairs;
    uint states;
    uint symbols;
    uint max_steps;
    uint rounds;
    uint num_actions;
    uint _pad[2];
};

// Run a TM on history bits as tape. Returns move (output_symbol % num_actions),
// or num_actions (sentinel = non-halt) on timeout.
// Tape layout: [0 0 ... 0 | history[0] ... history[hist_len-1]]
//               ^left_pad
// Head starts at rightmost history bit.
// Halts = move right past last position.
uint run_tm_inline(
    device const GpuTransition* trans,
    uint k,
    thread uchar* history,
    uint hist_len,
    uint max_steps,
    uint num_actions)
{
    const uint NONHALT = num_actions; // sentinel: value >= num_actions means non-halt

    // Allocate tape with left padding in thread-local memory
    const uint MAX_TM_TAPE = 4096;
    uchar tape[MAX_TM_TAPE];

    uint left_pad = min(max_steps + 1, MAX_TM_TAPE - hist_len - 1);
    uint tape_len = left_pad + hist_len;

    if (tape_len > MAX_TM_TAPE) {
        return NONHALT;
    }

    // Zero the left padding
    for (uint i = 0; i < left_pad; i++) {
        tape[i] = 0;
    }
    // Copy history bits as tape data
    for (uint i = 0; i < hist_len; i++) {
        tape[left_pad + i] = history[i];
    }

    uint head = tape_len - 1; // rightmost history bit
    uint state = 1;

    for (uint step = 0; step < max_steps; step++) {
        uchar read_sym = tape[head];
        uint idx = (state - 1) * k + read_sym;
        GpuTransition t = trans[idx];

        tape[head] = (uchar)t.write;

        if (t.move_right != 0u) {
            if (head + 1 == tape_len) {
                // Halted: return output mod num_actions
                return tape[tape_len - 1] % num_actions;
            }
            head++;
        } else {
            if (head == 0) {
                return NONHALT; // ran off left edge
            }
            head--;
        }

        state = t.next;
        if (state == 0u) {
            return NONHALT;
        }
    }

    return NONHALT; // timeout
}

kernel void tm_tournament(
    device const GpuTransition* all_transitions [[buffer(0)]],
    device const uint2* pairs                   [[buffer(1)]],
    device int2* scores                         [[buffer(2)]],
    constant GameParams& params                 [[buffer(3)]],
    device const int* payoff                    [[buffer(4)]],
    device atomic_uint* failed_flags            [[buffer(5)]],
    uint gid [[thread_position_in_grid]])
{
    if (gid >= params.num_pairs) return;

    uint a_idx = pairs[gid].x;
    uint b_idx = pairs[gid].y;
    uint k = params.symbols;
    uint na = params.num_actions;
    uint trans_per_tm = params.states * k;

    device const GpuTransition* trans_a = all_transitions + a_idx * trans_per_tm;
    device const GpuTransition* trans_b = all_transitions + b_idx * trans_per_tm;

    // History: flat bits [a1, b1, a2, b2, ...]
    // Max 1024 entries = 512 rounds
    uchar history[1024];
    uint hist_len = 0;

    int score_a = 0;
    int score_b = 0;

    for (uint round = 0; round < params.rounds; round++) {
        uint move_a, move_b;

        if (round == 0) {
            move_a = 0;
            move_b = 0; // cooperate on empty history
        } else {
            move_a = run_tm_inline(trans_a, k, history, hist_len, params.max_steps, na);
            move_b = run_tm_inline(trans_b, k, history, hist_len, params.max_steps, na);

            // Detect non-halting (sentinel = na means non-halt)
            if (move_a >= na) {
                atomic_fetch_or_explicit(&failed_flags[a_idx], 1u, memory_order_relaxed);
                move_a = 1; // default to defect for scoring
            }
            if (move_b >= na) {
                atomic_fetch_or_explicit(&failed_flags[b_idx], 1u, memory_order_relaxed);
                move_b = 1;
            }
        }

        // Append moves to history
        if (hist_len < 1024) {
            history[hist_len++] = (uchar)move_a;
        }
        if (hist_len < 1024) {
            history[hist_len++] = (uchar)move_b;
        }

        // Score: idx = move_a * na + move_b
        uint idx = move_a * na + move_b;
        score_a += payoff[idx * 2];
        score_b += payoff[idx * 2 + 1];
    }

    scores[gid] = int2(score_a, score_b);
}
"#;

    pub struct MetalSearcher {
        device: Device,
        queue: CommandQueue,
        pipeline: ComputePipelineState,
    }

    impl MetalSearcher {
        pub fn new() -> Result<Self, String> {
            let device =
                Device::system_default().ok_or("Metal device unavailable")?;
            let options = CompileOptions::new();
            let library = device
                .new_library_with_source(SHADER_SOURCE, &options)
                .map_err(|e| format!("Metal shader compile: {e}"))?;
            let func = library
                .get_function("tm_halting_test", None)
                .map_err(|e| format!("Metal function: {e}"))?;
            let pipeline = device
                .new_compute_pipeline_state_with_function(&func)
                .map_err(|e| format!("Metal pipeline: {e}"))?;
            let queue = device.new_command_queue();
            Ok(Self {
                device,
                queue,
                pipeline,
            })
        }

        /// Test a batch of TMs on a set of inputs. Returns a Vec<bool> per TM:
        /// true = all inputs halted, false = at least one didn't.
        /// `per_tm_step_limits` gives each TM its own step cap (e.g. worst_steps*2).
        pub fn test_batch(
            &self,
            transitions: &[Vec<super::super::TmTransition>], // one Vec per TM
            inputs: &[u64],
            states: u16,
            symbols: u8,
            max_steps: u32,
            input_width: u32,
            per_tm_step_limits: &[u32],
        ) -> Result<Vec<bool>, String> {
            let num_tms = transitions.len();
            let num_inputs = inputs.len();
            if num_tms == 0 || num_inputs == 0 {
                return Ok(vec![true; num_tms]);
            }

            // Flatten transitions into GPU format
            let gpu_trans: Vec<GpuTransition> = transitions
                .iter()
                .flat_map(|tm| {
                    tm.iter().map(|t| GpuTransition {
                        write: t.write as u32,
                        move_right: if t.move_right { 1 } else { 0 },
                        next: t.next as u32,
                        _pad: 0,
                    })
                })
                .collect();

            let gpu_inputs: Vec<u32> = inputs.iter().map(|&v| v as u32).collect();

            let params = GpuParams {
                num_tms: num_tms as u32,
                states: states as u32,
                symbols: symbols as u32,
                max_steps,
                num_inputs: num_inputs as u32,
                input_width,
            };

            // Create buffers
            let trans_buf = self.buf_from_slice(&gpu_trans);
            let input_buf = self.buf_from_slice(&gpu_inputs);
            let limits_buf = self.buf_from_slice(per_tm_step_limits);
            // halted_flags: one u32 per TM. 0 = all halted, non-zero = some failed.
            let flags_buf = self.device.new_buffer(
                (num_tms.max(1) * size_of::<u32>()) as u64,
                MTLResourceOptions::StorageModeShared,
            );
            // Zero the flags
            unsafe {
                std::ptr::write_bytes(
                    flags_buf.contents() as *mut u8,
                    0,
                    num_tms * size_of::<u32>(),
                );
            }

            // Dispatch
            let total_threads = num_tms * num_inputs;
            let cmd_buf = self.queue.new_command_buffer();
            let encoder = cmd_buf.new_compute_command_encoder();
            encoder.set_compute_pipeline_state(&self.pipeline);
            encoder.set_buffer(0, Some(&trans_buf), 0);
            encoder.set_buffer(1, Some(&input_buf), 0);
            encoder.set_buffer(2, Some(&flags_buf), 0);
            encoder.set_bytes(
                3,
                size_of::<GpuParams>() as u64,
                (&params as *const GpuParams).cast(),
            );
            encoder.set_buffer(4, Some(&limits_buf), 0);

            let width = self.pipeline.thread_execution_width().max(1);
            let threads_per_group = MTLSize::new(width, 1, 1);
            let groups = MTLSize::new(
                (total_threads as u64 + width - 1) / width,
                1,
                1,
            );
            encoder.dispatch_thread_groups(groups, threads_per_group);
            encoder.end_encoding();
            cmd_buf.commit();
            cmd_buf.wait_until_completed();

            // Read results
            let flags = unsafe {
                let ptr = flags_buf.contents() as *const u32;
                std::slice::from_raw_parts(ptr, num_tms)
            };
            Ok(flags.iter().map(|&f| f == 0).collect())
        }

        fn buf_from_slice<T>(&self, slice: &[T]) -> metal::Buffer {
            if slice.is_empty() {
                return self.device.new_buffer(
                    4,
                    MTLResourceOptions::StorageModeShared,
                );
            }
            let len = (slice.len() * size_of::<T>()) as u64;
            self.device.new_buffer_with_data(
                slice.as_ptr() as *const c_void,
                len,
                MTLResourceOptions::StorageModeShared,
            )
        }
    }

    /// Metal tournament runner. Separate from MetalSearcher because it uses
    /// a different shader pipeline.
    pub struct MetalTournament {
        device: Device,
        queue: CommandQueue,
        pipeline: ComputePipelineState,
    }

    impl MetalTournament {
        pub fn new() -> Result<Self, String> {
            let device =
                Device::system_default().ok_or("Metal device unavailable")?;
            let options = CompileOptions::new();
            let library = device
                .new_library_with_source(TOURNAMENT_SHADER_SOURCE, &options)
                .map_err(|e| format!("Metal tournament shader compile: {e}"))?;
            let func = library
                .get_function("tm_tournament", None)
                .map_err(|e| format!("Metal tournament function: {e}"))?;
            let pipeline = device
                .new_compute_pipeline_state_with_function(&func)
                .map_err(|e| format!("Metal tournament pipeline: {e}"))?;
            let queue = device.new_command_queue();
            Ok(Self {
                device,
                queue,
                pipeline,
            })
        }

        /// Run a tournament on GPU. Returns (survivors, score_matrix) where
        /// survivors are indices of TMs that always halted, and score_matrix
        /// only includes survivors.
        pub fn run_tournament(
            &self,
            ids: &[u64],
            states: u16,
            symbols: u8,
            max_steps: u32,
            rounds: u32,
            num_actions: u32,
            payoff_entries: &[[i32; 2]],
        ) -> Result<(Vec<usize>, Vec<Vec<i64>>), String> {
            let n = ids.len();
            if n == 0 {
                return Ok((vec![], vec![]));
            }

            // Decode all TMs
            let transitions: Vec<Vec<super::super::TmTransition>> = ids
                .iter()
                .map(|&id| super::super::decode_tm(id, states, symbols))
                .collect();

            // Flatten transitions into GPU format
            let gpu_trans: Vec<GpuTransition> = transitions
                .iter()
                .flat_map(|tm| {
                    tm.iter().map(|t| GpuTransition {
                        write: t.write as u32,
                        move_right: if t.move_right { 1 } else { 0 },
                        next: t.next as u32,
                        _pad: 0,
                    })
                })
                .collect();

            // Build all ordered pairs (i, j) where i != j
            let mut pairs: Vec<[u32; 2]> = Vec::with_capacity(n * (n - 1));
            for i in 0..n {
                for j in 0..n {
                    if i != j {
                        pairs.push([i as u32, j as u32]);
                    }
                }
            }
            let num_pairs = pairs.len();

            // Flatten payoff entries into interleaved [score_a, score_b, ...] for GPU
            let payoff_flat: Vec<i32> = payoff_entries
                .iter()
                .flat_map(|&[a, b]| vec![a, b])
                .collect();

            let params = GameParams {
                num_pairs: num_pairs as u32,
                states: states as u32,
                symbols: symbols as u32,
                max_steps,
                rounds,
                num_actions,
                _pad: [0; 2],
            };

            // Create buffers
            let trans_buf = self.buf_from_slice(&gpu_trans);
            let pairs_buf = self.buf_from_slice(&pairs);
            let payoff_buf = self.buf_from_slice(&payoff_flat);
            let scores_buf = self.device.new_buffer(
                (num_pairs.max(1) * size_of::<[i32; 2]>()) as u64,
                MTLResourceOptions::StorageModeShared,
            );
            // Per-TM failed flags (atomic uint, zeroed)
            let failed_buf = self.device.new_buffer(
                (n.max(1) * size_of::<u32>()) as u64,
                MTLResourceOptions::StorageModeShared,
            );
            unsafe {
                std::ptr::write_bytes(scores_buf.contents() as *mut u8, 0,
                    num_pairs * size_of::<[i32; 2]>());
                std::ptr::write_bytes(failed_buf.contents() as *mut u8, 0,
                    n * size_of::<u32>());
            }

            // Dispatch
            let cmd_buf = self.queue.new_command_buffer();
            let encoder = cmd_buf.new_compute_command_encoder();
            encoder.set_compute_pipeline_state(&self.pipeline);
            encoder.set_buffer(0, Some(&trans_buf), 0);
            encoder.set_buffer(1, Some(&pairs_buf), 0);
            encoder.set_buffer(2, Some(&scores_buf), 0);
            encoder.set_bytes(
                3,
                size_of::<GameParams>() as u64,
                (&params as *const GameParams).cast(),
            );
            encoder.set_buffer(4, Some(&payoff_buf), 0);
            encoder.set_buffer(5, Some(&failed_buf), 0);

            let width = self.pipeline.thread_execution_width().max(1);
            let threads_per_group = MTLSize::new(width, 1, 1);
            let groups = MTLSize::new(
                (num_pairs as u64 + width - 1) / width,
                1,
                1,
            );
            encoder.dispatch_thread_groups(groups, threads_per_group);
            encoder.end_encoding();
            cmd_buf.commit();
            cmd_buf.wait_until_completed();

            // Read results
            let raw_scores = unsafe {
                let ptr = scores_buf.contents() as *const [i32; 2];
                std::slice::from_raw_parts(ptr, num_pairs)
            };
            let raw_failed = unsafe {
                let ptr = failed_buf.contents() as *const u32;
                std::slice::from_raw_parts(ptr, n)
            };

            // Determine survivors (TMs that never failed to halt)
            let failed: std::collections::HashSet<usize> = raw_failed.iter()
                .enumerate()
                .filter(|(_, &f)| f != 0)
                .map(|(i, _)| i)
                .collect();

            if !failed.is_empty() {
                let failed_ids: Vec<u64> = failed.iter().map(|&i| ids[i]).collect();
                eprintln!("  GPU excluded {} non-halting TMs: {:?}", failed.len(), failed_ids);
            }

            let survivors: Vec<usize> = (0..n).filter(|i| !failed.contains(i)).collect();
            let m = survivors.len();

            // Build compact score matrix with only survivors
            let mut matrix = vec![vec![0i64; m]; m];
            let mut pair_idx = 0;
            for i in 0..n {
                for j in 0..n {
                    if i != j {
                        if !failed.contains(&i) && !failed.contains(&j) {
                            let si = survivors.iter().position(|&x| x == i).unwrap();
                            let sj = survivors.iter().position(|&x| x == j).unwrap();
                            matrix[si][sj] = raw_scores[pair_idx][0] as i64;
                        }
                        pair_idx += 1;
                    }
                }
            }

            Ok((survivors, matrix))
        }

        fn buf_from_slice<T>(&self, slice: &[T]) -> metal::Buffer {
            if slice.is_empty() {
                return self.device.new_buffer(
                    4,
                    MTLResourceOptions::StorageModeShared,
                );
            }
            let len = (slice.len() * size_of::<T>()) as u64;
            self.device.new_buffer_with_data(
                slice.as_ptr() as *const c_void,
                len,
                MTLResourceOptions::StorageModeShared,
            )
        }
    }

    // ── CA Tournament on Metal ─────────────────────────────────────────

    const CA_TOURNAMENT_SHADER: &str = r#"
#include <metal_stdlib>
using namespace metal;

struct CaGameParams {
    uint num_pairs;
    uint k;
    uint two_r;
    uint t;          // CA evolution steps
    uint rounds;
    uint num_actions;
    uint table_len;
    uint _pad;
};

// Run shrinking CA on history, return last cell % num_actions.
// Uses thread-local arrays for the row computation.
uint run_ca_inline(
    device const uchar* rule_table,
    uint k,
    uint two_r,
    uint t_steps,
    thread uchar* history,
    uint hist_len,
    uint num_actions)
{
    if (hist_len == 0) return 0;

    const uint MAX_ROW = 512;
    uchar row[MAX_ROW];
    uint row_len = min(hist_len, MAX_ROW);

    for (uint i = 0; i < row_len; i++) {
        row[i] = history[i];
    }

    uint neighborhood = two_r + 1;

    for (uint step = 0; step < t_steps; step++) {
        if (row_len <= two_r) break;
        uint next_len = row_len - two_r;
        if (next_len == 0) break;

        uchar next_row[MAX_ROW];
        for (uint pos = 0; pos < next_len; pos++) {
            uint idx = 0;
            for (uint n = 0; n < neighborhood; n++) {
                idx = idx * k + row[pos + n];
            }
            next_row[pos] = rule_table[idx];
        }

        for (uint i = 0; i < next_len; i++) {
            row[i] = next_row[i];
        }
        row_len = next_len;
    }

    return row[row_len - 1] % num_actions;
}

kernel void ca_tournament(
    device const uchar* all_rule_tables [[buffer(0)]],
    device const uint2* pairs           [[buffer(1)]],
    device int2* scores                 [[buffer(2)]],
    constant CaGameParams& params       [[buffer(3)]],
    device const int* payoff            [[buffer(4)]],
    uint gid [[thread_position_in_grid]])
{
    if (gid >= params.num_pairs) return;

    uint a_idx = pairs[gid].x;
    uint b_idx = pairs[gid].y;

    device const uchar* table_a = all_rule_tables + a_idx * params.table_len;
    device const uchar* table_b = all_rule_tables + b_idx * params.table_len;

    uint na = params.num_actions;

    uchar history[1024];
    uint hist_len = 0;
    int score_a = 0;
    int score_b = 0;

    for (uint round = 0; round < params.rounds; round++) {
        uint move_a, move_b;

        if (round == 0) {
            move_a = 0;
            move_b = 0;
        } else {
            move_a = run_ca_inline(table_a, params.k, params.two_r, params.t, history, hist_len, na);
            move_b = run_ca_inline(table_b, params.k, params.two_r, params.t, history, hist_len, na);
        }

        if (hist_len < 1024) { history[hist_len++] = (uchar)move_a; }
        if (hist_len < 1024) { history[hist_len++] = (uchar)move_b; }

        uint idx = move_a * na + move_b;
        score_a += payoff[idx * 2];
        score_b += payoff[idx * 2 + 1];
    }

    scores[gid] = int2(score_a, score_b);
}
"#;

    #[repr(C)]
    #[derive(Copy, Clone)]
    struct CaGameParams {
        num_pairs: u32,
        k: u32,
        two_r: u32,
        t: u32,
        rounds: u32,
        num_actions: u32,
        table_len: u32,
        _pad: u32,
    }

    pub struct MetalCaTournament {
        device: Device,
        queue: CommandQueue,
        pipeline: ComputePipelineState,
    }

    impl MetalCaTournament {
        pub fn new() -> Result<Self, String> {
            let device =
                Device::system_default().ok_or("Metal device unavailable")?;
            let options = CompileOptions::new();
            let library = device
                .new_library_with_source(CA_TOURNAMENT_SHADER, &options)
                .map_err(|e| format!("Metal CA tournament shader compile: {e}"))?;
            let func = library
                .get_function("ca_tournament", None)
                .map_err(|e| format!("Metal CA tournament function: {e}"))?;
            let pipeline = device
                .new_compute_pipeline_state_with_function(&func)
                .map_err(|e| format!("Metal CA tournament pipeline: {e}"))?;
            let queue = device.new_command_queue();
            Ok(Self {
                device,
                queue,
                pipeline,
            })
        }

        pub fn run_tournament(
            &self,
            rule_ids: &[u64],
            k: u8,
            two_r: u32,
            t: u32,
            rounds: u32,
            num_actions: u32,
            payoff_entries: &[[i32; 2]],
        ) -> Result<Vec<Vec<i64>>, String> {
            let n = rule_ids.len();
            if n == 0 {
                return Ok(vec![]);
            }

            let table_len = (k as u32).pow(two_r + 1) as usize;

            // Decode all rule tables on CPU
            let flat_tables: Vec<u8> = rule_ids
                .iter()
                .flat_map(|&rule| {
                    super::super::strategy::decode_ca_rule_table(rule, k, two_r)
                })
                .collect();

            // Build all ordered pairs (i, j) where i != j
            let mut all_pairs: Vec<[u32; 2]> = Vec::with_capacity(n * (n - 1));
            for i in 0..n {
                for j in 0..n {
                    if i != j {
                        all_pairs.push([i as u32, j as u32]);
                    }
                }
            }

            // Flatten payoff entries into interleaved [score_a, score_b, ...] for GPU
            let payoff_flat: Vec<i32> = payoff_entries
                .iter()
                .flat_map(|&[a, b]| vec![a, b])
                .collect();

            // Upload rule tables and payoff once (shared across all batches)
            let tables_buf = self.buf_from_slice(&flat_tables);
            let payoff_buf = self.buf_from_slice(&payoff_flat);

            // Process pairs in batches to limit GPU memory usage
            const BATCH_SIZE: usize = 5_000_000;
            let mut matrix = vec![vec![0i64; n]; n];

            for (batch_idx, batch) in all_pairs.chunks(BATCH_SIZE).enumerate() {
                let batch_len = batch.len();
                let params = CaGameParams {
                    num_pairs: batch_len as u32,
                    k: k as u32,
                    two_r,
                    t,
                    rounds,
                    num_actions,
                    table_len: table_len as u32,
                    _pad: 0,
                };

                let pairs_buf = self.buf_from_slice(batch);
                let scores_buf = self.device.new_buffer(
                    (batch_len.max(1) * size_of::<[i32; 2]>()) as u64,
                    MTLResourceOptions::StorageModeShared,
                );
                unsafe {
                    std::ptr::write_bytes(
                        scores_buf.contents() as *mut u8,
                        0,
                        batch_len * size_of::<[i32; 2]>(),
                    );
                }

                let cmd_buf = self.queue.new_command_buffer();
                let encoder = cmd_buf.new_compute_command_encoder();
                encoder.set_compute_pipeline_state(&self.pipeline);
                encoder.set_buffer(0, Some(&tables_buf), 0);
                encoder.set_buffer(1, Some(&pairs_buf), 0);
                encoder.set_buffer(2, Some(&scores_buf), 0);
                encoder.set_bytes(
                    3,
                    size_of::<CaGameParams>() as u64,
                    (&params as *const CaGameParams).cast(),
                );
                encoder.set_buffer(4, Some(&payoff_buf), 0);

                let width = self.pipeline.thread_execution_width().max(1);
                let threads_per_group = MTLSize::new(width, 1, 1);
                let groups = MTLSize::new(
                    (batch_len as u64 + width - 1) / width,
                    1,
                    1,
                );
                encoder.dispatch_thread_groups(groups, threads_per_group);
                encoder.end_encoding();
                cmd_buf.commit();
                cmd_buf.wait_until_completed();

                // Read batch results into score matrix
                let raw_scores = unsafe {
                    let ptr = scores_buf.contents() as *const [i32; 2];
                    std::slice::from_raw_parts(ptr, batch_len)
                };

                let batch_start = batch_idx * BATCH_SIZE;
                for (local_idx, score) in raw_scores.iter().enumerate() {
                    let pair = all_pairs[batch_start + local_idx];
                    matrix[pair[0] as usize][pair[1] as usize] = score[0] as i64;
                }
            }

            Ok(matrix)
        }

        fn buf_from_slice<T>(&self, slice: &[T]) -> metal::Buffer {
            if slice.is_empty() {
                return self.device.new_buffer(
                    4,
                    MTLResourceOptions::StorageModeShared,
                );
            }
            let len = (slice.len() * size_of::<T>()) as u64;
            self.device.new_buffer_with_data(
                slice.as_ptr() as *const c_void,
                len,
                MTLResourceOptions::StorageModeShared,
            )
        }
    }

    // ── CA Classification on Metal ──────────────────────────────────────

    const CA_CLASSIFY_SHADER: &str = r#"
#include <metal_stdlib>
using namespace metal;

struct CaClassifyParams {
    uint total_rules;
    uint k;
    uint two_r;
    uint t;
    uint depth;
    uint sig_len;
    uint table_len;
};

#define CA_MAX_ROW   64
#define CA_MAX_TABLE 256

kernel void ca_classify(
    device uchar* signatures         [[buffer(0)]],
    constant CaClassifyParams& params [[buffer(1)]],
    uint gid [[thread_position_in_grid]])
{
    if (gid >= params.total_rules) return;

    uint rule_code = gid;
    uint k = params.k;
    uint two_r = params.two_r;
    uint t_steps = params.t;
    uint depth = params.depth;
    uint neighborhood = two_r + 1;
    uint table_len = params.table_len;

    // Decode rule table: IntegerDigits[rule_code, k, table_len]
    uchar rule_table[CA_MAX_TABLE];
    {
        uint tmp = rule_code;
        for (int i = (int)table_len - 1; i >= 0; i--) {
            rule_table[i] = tmp % k;
            tmp /= k;
        }
    }

    // Pointer to this rule's signature slice
    ulong offset = (ulong)gid * (ulong)params.sig_len;
    device uchar* my_sig = signatures + offset;
    uint sig_idx = 0;

    for (uint history_len = 0; history_len <= depth * 2; history_len++) {
        uint num_histories;
        if (history_len == 0) {
            num_histories = 1;
        } else {
            num_histories = min(1u << min(history_len, 20u), 256u);
        }

        for (uint h = 0; h < num_histories; h++) {
            // Empty history -> cooperate
            if (history_len == 0) {
                my_sig[sig_idx++] = 0;
                continue;
            }

            // Build initial row from history bits (LSB-first, matching CPU)
            uchar row[CA_MAX_ROW];
            uint row_len = min(history_len, (uint)CA_MAX_ROW);

            for (uint bit = 0; bit < row_len; bit++) {
                row[bit] = (h >> bit) & 1;
            }

            // Run shrinking CA
            for (uint step = 0; step < t_steps; step++) {
                if (row_len <= two_r) break;
                uint next_len = row_len - two_r;
                if (next_len == 0) break;

                uchar next_row[CA_MAX_ROW];
                for (uint pos = 0; pos < next_len; pos++) {
                    uint idx = 0;
                    for (uint n = 0; n < neighborhood; n++) {
                        idx = idx * k + row[pos + n];
                    }
                    next_row[pos] = (idx < table_len) ? rule_table[idx] : 0;
                }

                for (uint i = 0; i < next_len; i++) {
                    row[i] = next_row[i];
                }
                row_len = next_len;
            }

            my_sig[sig_idx++] = row[row_len - 1] % 2;
        }
    }
}
"#;

    #[repr(C)]
    #[derive(Copy, Clone)]
    struct CaClassifyParams {
        total_rules: u32,
        k: u32,
        two_r: u32,
        t: u32,
        depth: u32,
        sig_len: u32,
        table_len: u32,
    }

    pub struct MetalCaClassifier {
        device: Device,
        queue: CommandQueue,
        pipeline: ComputePipelineState,
    }

    impl MetalCaClassifier {
        pub fn new() -> Result<Self, String> {
            let device =
                Device::system_default().ok_or("Metal device unavailable")?;
            let options = CompileOptions::new();
            let library = device
                .new_library_with_source(CA_CLASSIFY_SHADER, &options)
                .map_err(|e| format!("Metal CA shader compile: {e}"))?;
            let func = library
                .get_function("ca_classify", None)
                .map_err(|e| format!("Metal CA function: {e}"))?;
            let pipeline = device
                .new_compute_pipeline_state_with_function(&func)
                .map_err(|e| format!("Metal CA pipeline: {e}"))?;
            let queue = device.new_command_queue();
            Ok(Self {
                device,
                queue,
                pipeline,
            })
        }

        /// Classify all CA rules on GPU. Returns flat signature buffer and sig_len.
        pub fn classify(
            &self,
            total_rules: u32,
            k: u8,
            two_r: u32,
            t: u32,
            depth: u32,
        ) -> Result<(Vec<u8>, usize), String> {
            let sig_len = super::compute_ca_sig_len(depth);
            let table_len = (k as u32).pow(two_r + 1);
            let buf_bytes = (total_rules as u64) * (sig_len as u64);

            if buf_bytes == 0 {
                return Ok((vec![], sig_len as usize));
            }

            let params = CaClassifyParams {
                total_rules,
                k: k as u32,
                two_r,
                t,
                depth,
                sig_len,
                table_len,
            };

            // Allocate output buffer
            let sig_buf = self.device.new_buffer(
                buf_bytes.max(4),
                MTLResourceOptions::StorageModeShared,
            );

            // Dispatch
            let cmd_buf = self.queue.new_command_buffer();
            let encoder = cmd_buf.new_compute_command_encoder();
            encoder.set_compute_pipeline_state(&self.pipeline);
            encoder.set_buffer(0, Some(&sig_buf), 0);
            encoder.set_bytes(
                1,
                size_of::<CaClassifyParams>() as u64,
                (&params as *const CaClassifyParams).cast(),
            );

            let width = self.pipeline.thread_execution_width().max(1);
            let threads_per_group = MTLSize::new(width, 1, 1);
            let groups = MTLSize::new(
                (total_rules as u64 + width - 1) / width,
                1,
                1,
            );
            encoder.dispatch_thread_groups(groups, threads_per_group);
            encoder.end_encoding();
            cmd_buf.commit();
            cmd_buf.wait_until_completed();

            // Read back
            let raw = unsafe {
                let ptr = sig_buf.contents() as *const u8;
                std::slice::from_raw_parts(ptr, buf_bytes as usize)
            };

            Ok((raw.to_vec(), sig_len as usize))
        }
    }

    // ── FSM Tournament on Metal ────────────────────────────────────────

    /// Build the FSM tournament shader source with the correct MAX_STATES define.
    fn fsm_tournament_shader_source(max_states: usize) -> String {
        format!(
            r#"
#include <metal_stdlib>
using namespace metal;

#define FSM_MAX_STATES {max_states}u

struct FsmGameParams {{
    uint num_pairs;
    uint states;
    uint alphabet;
    uint rounds;
    uint num_actions;
    uint _pad[3];
}};

kernel void fsm_tournament(
    device const uint* starts        [[buffer(0)]],
    device const uint* outputs       [[buffer(1)]],
    device const uint* transitions   [[buffer(2)]],
    device const uint2* pairs        [[buffer(3)]],
    device int2* scores              [[buffer(4)]],
    constant FsmGameParams& params   [[buffer(5)]],
    device const int* payoff         [[buffer(6)]],
    uint gid [[thread_position_in_grid]])
{{
    if (gid >= params.num_pairs) return;

    uint a_idx = pairs[gid].x;
    uint b_idx = pairs[gid].y;
    uint S = params.states;
    uint K = params.alphabet;
    uint na = params.num_actions;

    uint a_state = starts[a_idx];
    uint b_state = starts[b_idx];

    int score_a = 0;
    int score_b = 0;

    // Cycle detection: combined state = a_state * S + b_state
    // Maximum combined states = S * S. When we see a repeated combined
    // state, we have found a cycle and can extrapolate in O(1).
    const uint max_combined = FSM_MAX_STATES * FSM_MAX_STATES;
    uint cycle_round[FSM_MAX_STATES * FSM_MAX_STATES];
    int cycle_sa[FSM_MAX_STATES * FSM_MAX_STATES];
    int cycle_sb[FSM_MAX_STATES * FSM_MAX_STATES];
    for (uint i = 0u; i < max_combined; i++) {{
        cycle_round[i] = 0xFFFFFFFFu;
    }}

    for (uint round = 0u; round < params.rounds; round++) {{
        uint combined = a_state * S + b_state;
        if (combined < max_combined) {{
            if (cycle_round[combined] != 0xFFFFFFFFu) {{
                uint cycle_len = round - cycle_round[combined];
                if (cycle_len > 0u) {{
                    int delta_a = score_a - cycle_sa[combined];
                    int delta_b = score_b - cycle_sb[combined];
                    uint remaining = params.rounds - round;
                    uint full_cycles = remaining / cycle_len;
                    score_a += int(full_cycles) * delta_a;
                    score_b += int(full_cycles) * delta_b;
                    uint leftover = remaining - full_cycles * cycle_len;
                    for (uint r = 0u; r < leftover; r++) {{
                        uint aa = outputs[a_idx * S + a_state] % na;
                        uint ba = outputs[b_idx * S + b_state] % na;
                        uint pidx = aa * na + ba;
                        score_a += payoff[pidx * 2];
                        score_b += payoff[pidx * 2 + 1];
                        uint new_a = transitions[a_idx * S * K + a_state * K + ba];
                        uint new_b = transitions[b_idx * S * K + b_state * K + aa];
                        a_state = new_a;
                        b_state = new_b;
                    }}
                    scores[gid] = int2(score_a, score_b);
                    return;
                }}
            }}
            cycle_round[combined] = round;
            cycle_sa[combined] = score_a;
            cycle_sb[combined] = score_b;
        }}

        uint a_action = outputs[a_idx * S + a_state] % na;
        uint b_action = outputs[b_idx * S + b_state] % na;
        uint pidx = a_action * na + b_action;
        score_a += payoff[pidx * 2];
        score_b += payoff[pidx * 2 + 1];

        uint new_a = transitions[a_idx * S * K + a_state * K + b_action];
        uint new_b = transitions[b_idx * S * K + b_state * K + a_action];
        a_state = new_a;
        b_state = new_b;
    }}

    scores[gid] = int2(score_a, score_b);
}}
"#,
            max_states = max_states
        )
    }

    pub struct MetalFsmTournament {
        device: Device,
        queue: CommandQueue,
        pipeline: ComputePipelineState,
    }

    impl MetalFsmTournament {
        pub fn new(max_states: usize) -> Result<Self, String> {
            let device =
                Device::system_default().ok_or("Metal device unavailable")?;
            let source = fsm_tournament_shader_source(max_states);
            let options = CompileOptions::new();
            let library = device
                .new_library_with_source(&source, &options)
                .map_err(|e| format!("Metal FSM tournament shader compile: {e}"))?;
            let func = library
                .get_function("fsm_tournament", None)
                .map_err(|e| format!("Metal FSM tournament function: {e}"))?;
            let pipeline = device
                .new_compute_pipeline_state_with_function(&func)
                .map_err(|e| format!("Metal FSM tournament pipeline: {e}"))?;
            let queue = device.new_command_queue();
            Ok(Self {
                device,
                queue,
                pipeline,
            })
        }

        pub fn run_tournament(
            &self,
            fsm_ids: &[u64],
            states: usize,
            symbols: usize,
            rounds: u32,
            num_actions: u32,
            payoff_entries: &[[i32; 2]],
        ) -> Result<Vec<Vec<i64>>, String> {
            let n = fsm_ids.len();
            if n == 0 {
                return Ok(vec![]);
            }

            // Decode all FSMs
            let mut flat_starts: Vec<u32> = Vec::with_capacity(n);
            let mut flat_outputs: Vec<u32> = Vec::with_capacity(n * states);
            let mut flat_transitions: Vec<u32> = Vec::with_capacity(n * states * symbols);

            for &id in fsm_ids {
                let (outputs, transitions) =
                    super::super::strategy::decode_fsm(id, states, symbols);
                flat_starts.push(0u32); // all FSMs start in state 0
                for &o in &outputs {
                    flat_outputs.push(o as u32);
                }
                for &t in &transitions {
                    flat_transitions.push(t as u32);
                }
            }

            // Build all ordered pairs (i, j) where i != j
            let mut all_pairs: Vec<[u32; 2]> = Vec::with_capacity(n * (n - 1));
            for i in 0..n {
                for j in 0..n {
                    if i != j {
                        all_pairs.push([i as u32, j as u32]);
                    }
                }
            }

            // Flatten payoff entries into interleaved [score_a, score_b, ...] for GPU
            let payoff_flat: Vec<i32> = payoff_entries
                .iter()
                .flat_map(|&[a, b]| vec![a, b])
                .collect();

            #[repr(C)]
            #[derive(Copy, Clone)]
            struct FsmGameParams {
                num_pairs: u32,
                states: u32,
                alphabet: u32,
                rounds: u32,
                num_actions: u32,
                _pad: [u32; 3],
            }

            // Upload shared data once
            let starts_buf = self.buf_from_slice(&flat_starts);
            let outputs_buf = self.buf_from_slice(&flat_outputs);
            let transitions_buf = self.buf_from_slice(&flat_transitions);
            let payoff_buf = self.buf_from_slice(&payoff_flat);

            // Process pairs in batches
            const BATCH_SIZE: usize = 5_000_000;
            let mut matrix = vec![vec![0i64; n]; n];

            for (batch_idx, batch) in all_pairs.chunks(BATCH_SIZE).enumerate() {
                let batch_len = batch.len();
                let params = FsmGameParams {
                    num_pairs: batch_len as u32,
                    states: states as u32,
                    alphabet: symbols as u32,
                    rounds,
                    num_actions,
                    _pad: [0; 3],
                };

                let pairs_buf = self.buf_from_slice(batch);
                let scores_buf = self.device.new_buffer(
                    (batch_len.max(1) * size_of::<[i32; 2]>()) as u64,
                    MTLResourceOptions::StorageModeShared,
                );
                unsafe {
                    std::ptr::write_bytes(
                        scores_buf.contents() as *mut u8,
                        0,
                        batch_len * size_of::<[i32; 2]>(),
                    );
                }

                let cmd_buf = self.queue.new_command_buffer();
                let encoder = cmd_buf.new_compute_command_encoder();
                encoder.set_compute_pipeline_state(&self.pipeline);
                encoder.set_buffer(0, Some(&starts_buf), 0);
                encoder.set_buffer(1, Some(&outputs_buf), 0);
                encoder.set_buffer(2, Some(&transitions_buf), 0);
                encoder.set_buffer(3, Some(&pairs_buf), 0);
                encoder.set_buffer(4, Some(&scores_buf), 0);
                encoder.set_bytes(
                    5,
                    size_of::<FsmGameParams>() as u64,
                    (&params as *const FsmGameParams).cast(),
                );
                encoder.set_buffer(6, Some(&payoff_buf), 0);

                let width = self.pipeline.thread_execution_width().max(1);
                let threads_per_group = MTLSize::new(width, 1, 1);
                let groups = MTLSize::new(
                    (batch_len as u64 + width - 1) / width,
                    1,
                    1,
                );
                encoder.dispatch_thread_groups(groups, threads_per_group);
                encoder.end_encoding();
                cmd_buf.commit();
                cmd_buf.wait_until_completed();

                // Read batch results into score matrix
                let raw_scores = unsafe {
                    let ptr = scores_buf.contents() as *const [i32; 2];
                    std::slice::from_raw_parts(ptr, batch_len)
                };

                let batch_start = batch_idx * BATCH_SIZE;
                for (local_idx, score) in raw_scores.iter().enumerate() {
                    let pair = all_pairs[batch_start + local_idx];
                    matrix[pair[0] as usize][pair[1] as usize] = score[0] as i64;
                }
            }

            Ok(matrix)
        }

        fn buf_from_slice<T>(&self, slice: &[T]) -> metal::Buffer {
            if slice.is_empty() {
                return self.device.new_buffer(
                    4,
                    MTLResourceOptions::StorageModeShared,
                );
            }
            let len = (slice.len() * size_of::<T>()) as u64;
            self.device.new_buffer_with_data(
                slice.as_ptr() as *const c_void,
                len,
                MTLResourceOptions::StorageModeShared,
            )
        }
    }
}

// Re-export
#[cfg(all(target_os = "macos", feature = "metal"))]
pub use metal_impl::MetalSearcher;

/// Run a tournament on GPU. Public wrapper that handles Metal init.
/// Returns (survivors, score_matrix) — same as CPU tournament.
#[cfg(all(target_os = "macos", feature = "metal"))]
pub fn run_tournament_gpu(
    ids: &[u64],
    states: u16,
    symbols: u8,
    max_steps: u32,
    rounds: u32,
    num_actions: u32,
    payoff_entries: &[[i32; 2]],
) -> Result<(Vec<usize>, Vec<Vec<i64>>), String> {
    let gpu = metal_impl::MetalTournament::new()?;
    gpu.run_tournament(ids, states, symbols, max_steps, rounds, num_actions, payoff_entries)
}

#[cfg(not(all(target_os = "macos", feature = "metal")))]
pub fn run_tournament_gpu(
    _ids: &[u64],
    _states: u16,
    _symbols: u8,
    _max_steps: u32,
    _rounds: u32,
    _num_actions: u32,
    _payoff_entries: &[[i32; 2]],
) -> Result<(Vec<usize>, Vec<Vec<i64>>), String> {
    Err("Metal not available".to_string())
}

/// Run a CA tournament on GPU. Returns score_matrix[i][j].
#[cfg(all(target_os = "macos", feature = "metal"))]
pub fn run_ca_tournament_gpu(
    rule_ids: &[u64],
    k: u8,
    two_r: u32,
    t: u32,
    rounds: u32,
    num_actions: u32,
    payoff_entries: &[[i32; 2]],
) -> Result<Vec<Vec<i64>>, String> {
    let gpu = metal_impl::MetalCaTournament::new()?;
    gpu.run_tournament(rule_ids, k, two_r, t, rounds, num_actions, payoff_entries)
}

#[cfg(not(all(target_os = "macos", feature = "metal")))]
pub fn run_ca_tournament_gpu(
    _rule_ids: &[u64],
    _k: u8,
    _two_r: u32,
    _t: u32,
    _rounds: u32,
    _num_actions: u32,
    _payoff_entries: &[[i32; 2]],
) -> Result<Vec<Vec<i64>>, String> {
    Err("Metal not available".to_string())
}

/// Classify all CA rules on GPU. Returns (flat_signatures, sig_len).
/// The caller slices flat_signatures into chunks of sig_len per rule.
#[cfg(all(target_os = "macos", feature = "metal"))]
pub fn classify_ca_gpu(
    total_rules: u32,
    k: u8,
    two_r: u32,
    t: u32,
    depth: u32,
) -> Result<(Vec<u8>, usize), String> {
    let gpu = metal_impl::MetalCaClassifier::new()?;
    gpu.classify(total_rules, k, two_r, t, depth)
}

#[cfg(not(all(target_os = "macos", feature = "metal")))]
pub fn classify_ca_gpu(
    _total_rules: u32,
    _k: u8,
    _two_r: u32,
    _t: u32,
    _depth: u32,
) -> Result<(Vec<u8>, usize), String> {
    Err("Metal not available".to_string())
}

/// Compute the total signature length for CA classification at a given depth.
pub fn compute_ca_sig_len(depth: u32) -> u32 {
    let mut len = 0u32;
    for history_len in 0..=(depth * 2) {
        if history_len == 0 {
            len += 1;
        } else {
            len += (2u64.pow(history_len.min(20))).min(256) as u32;
        }
    }
    len
}

/// Run an FSM tournament on GPU. Returns score_matrix[i][j].
#[cfg(all(target_os = "macos", feature = "metal"))]
pub fn run_fsm_tournament_gpu(
    fsm_ids: &[u64],
    states: usize,
    symbols: usize,
    rounds: u32,
    num_actions: u32,
    payoff_entries: &[[i32; 2]],
) -> Result<Vec<Vec<i64>>, String> {
    let gpu = metal_impl::MetalFsmTournament::new(states)?;
    gpu.run_tournament(fsm_ids, states, symbols, rounds, num_actions, payoff_entries)
}

#[cfg(not(all(target_os = "macos", feature = "metal")))]
pub fn run_fsm_tournament_gpu(
    _fsm_ids: &[u64],
    _states: usize,
    _symbols: usize,
    _rounds: u32,
    _num_actions: u32,
    _payoff_entries: &[[i32; 2]],
) -> Result<Vec<Vec<i64>>, String> {
    Err("Metal not available".to_string())
}

/// Check if Metal is available at runtime.
#[allow(dead_code)]
pub fn metal_available() -> bool {
    #[cfg(all(target_os = "macos", feature = "metal"))]
    {
        metal::Device::system_default().is_some()
    }
    #[cfg(not(all(target_os = "macos", feature = "metal")))]
    {
        false
    }
}
