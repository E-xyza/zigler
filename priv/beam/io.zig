//! BEAM-based Io implementation for Zig 0.16.0
//!
//! This module provides an std.Io interface backed by std.Io.Threaded
//! configured for BEAM NIF use, with BEAM-specific overrides:
//!
//! - Time: Uses enif_monotonic_time/enif_time_offset for BEAM-consistent time
//! - Stderr: No locking (can't coordinate with BEAM's stderr)
//!
//! Configuration:
//! - Uses beam.allocator for memory allocation
//! - environ is disabled - use Erlang's os:getenv/1 or Elixir's System.get_env/1
//!   and pass the value into the NIF function as a parameter
//! - Signal handlers are disabled (BEAM manages signals)

const std = @import("std");
const beam = @import("beam.zig");
const e = @import("erl_nif");

const Io = std.Io;
const Threaded = Io.Threaded;

/// Global Threaded instance - initialized in on_load
var global_threaded: ?Threaded = null;

/// Our custom vtable - copied from Threaded with selective overrides
var vtable: Io.VTable = undefined;

/// Initialize the global Io instance.
/// Called from on_load.
pub fn initGlobal() void {
    global_threaded = .{
        .allocator = beam.allocator,
        .stack_size = std.Thread.SpawnConfig.default_stack_size,
        .async_limit = .nothing,
        .cpu_count_error = null,
        .concurrent_limit = .nothing,
        .old_sig_io = undefined,
        .old_sig_pipe = undefined,
        .have_signal_handler = false,
        .argv0 = .empty,
        .environ_initialized = false, // Use os:getenv/System.get_env and pass as parameter
        .environ = .{ .process_environ = .{ .block = .empty } },
        .worker_threads = .init(null),
        .disable_memory_mapping = false,
    };

    // Copy Threaded's vtable as our base
    vtable = global_threaded.?.io().vtable.*;

    // Apply BEAM-specific overrides
    vtable.now = now;
    vtable.clockResolution = clockResolution;
    vtable.lockStderr = lockStderr;
    vtable.tryLockStderr = tryLockStderr;
    vtable.unlockStderr = unlockStderr;
}

pub fn deinitGlobal() void {
    if (global_threaded) |*g| {
        g.deinit();
        global_threaded = null;
    }
}

/// Get the std.Io interface with our custom vtable.
/// The allocator is used for Io operations that require allocation.
pub fn get(allocator: std.mem.Allocator) Io {
    global_threaded.?.allocator = allocator;
    return .{
        .userdata = @ptrCast(&global_threaded.?),
        .vtable = &vtable,
    };
}

// ============================================================================
// BEAM-specific implementations
// ============================================================================

/// Time using BEAM's enif_monotonic_time / enif_time_offset
/// This ensures time is consistent with BEAM's view of time.
fn now(userdata: ?*anyopaque, clock: Io.Clock) Io.Timestamp {
    const beam_time: i64 = switch (clock) {
        // awake = monotonic time (excluding suspend)
        // boot = monotonic time (including suspend)
        .awake, .boot => e.enif_monotonic_time(e.ERL_NIF_NSEC),
        // real = wall clock time
        .real => blk: {
            // BEAM system time = monotonic + offset
            const mono = e.enif_monotonic_time(e.ERL_NIF_NSEC);
            const offset = e.enif_time_offset(e.ERL_NIF_NSEC);
            break :blk mono + offset;
        },
        // CPU time - no BEAM equivalent, fall back to Threaded
        .cpu_process, .cpu_thread => {
            const threaded: *Threaded = @ptrCast(@alignCast(userdata.?));
            return threaded.io().vtable.now(userdata, clock);
        },
    };

    return .{ .nanoseconds = @as(i96, beam_time) };
}

/// Clock resolution - BEAM time functions provide nanosecond precision
fn clockResolution(userdata: ?*anyopaque, clock: Io.Clock) Io.Clock.ResolutionError!Io.Duration {
    switch (clock) {
        .cpu_process, .cpu_thread => {
            // Fall back to Threaded for CPU time clocks
            const threaded: *Threaded = @ptrCast(@alignCast(userdata.?));
            return threaded.io().vtable.clockResolution(userdata, clock);
        },
        else => {
            // BEAM time resolution is nanosecond
            return .{ .nanoseconds = 1 };
        },
    }
}

/// Stderr - no locking (can't coordinate with BEAM's stderr)
fn lockStderr(userdata: ?*anyopaque, mode: ?Io.Terminal.Mode) Io.Cancelable!Io.LockedStderr {
    const threaded: *Threaded = @ptrCast(@alignCast(userdata.?));
    // Return stderr without acquiring any lock
    return .{
        .file_writer = &threaded.stderr_writer,
        .terminal_mode = mode orelse threaded.stderr_mode,
    };
}

fn tryLockStderr(userdata: ?*anyopaque, mode: ?Io.Terminal.Mode) Io.Cancelable!?Io.LockedStderr {
    // Always succeeds since we don't lock
    return try lockStderr(userdata, mode);
}

fn unlockStderr(_: ?*anyopaque) void {
    // No-op since we don't lock
}

// Note: Other functions we considered but left with Threaded defaults:
//
// - sleep: Could use enif_consume_timeslice for cooperative sleeping,
//   but that changes semantics significantly. Threaded uses posix nanosleep.
//
// - futexWait/futexWake: Could implement with BEAM cond/mutex, but the
//   semantics are complex. Threaded's posix futex implementation works.
//
// - random/randomSecure: Could use enif_make_unique_integer, but
//   Threaded's implementation is more appropriate for crypto-quality random.
//
// - environ: Disabled via environ_initialized=false. Use Erlang's os:getenv/1
//   or Elixir's System.get_env/1 and pass values into the NIF as parameters.
