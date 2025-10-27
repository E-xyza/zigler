const std = @import("std");
const beam = @import("beam.zig");
const e = @import("erl_nif");
const options = @import("options.zig");

const BeamThreadFn = *const fn (?*anyopaque) callconv(.c) ?*anyopaque;

pub const ThreadError = error{ threaderror, threadtooktoolong, processnotjoined, processterminated };

pub const ThreadState = enum {
    const This = @This();
    prepped, // harness has created the thread and is waiting for thread to check in.
    running, // thread has checked in
    finished, // thread function has completed running
    joining, // some thread is trying to join this thread
    joined, // all activity on the thread has completed.
    failed, // something bad and unrecoverable has happened.

    pub fn set(self: *This, state: ThreadState) void {
        @atomicStore(This, self, state, .monotonic);
    }

    pub fn get(self: *This) ThreadState {
        return @atomicLoad(This, self, .monotonic);
    }

    pub fn exchange(self: *This, old: ThreadState, new: ThreadState) ?ThreadState {
        return @cmpxchgStrong(This, self, old, new, .monotonic, .monotonic);
    }

    pub fn wait_while(self: *This, state: ThreadState) void {
        while (self.get() == state) {
            std.Thread.sleep(1000);
        }
    }

    fn check_against(state: This, state_or_states: anytype) bool {
        switch (@typeInfo(@TypeOf(state_or_states))) {
            .@"struct" => {
                inline for (state_or_states) |check| {
                    if (state == check) return true;
                }
                return false;
            },
            else => return state == state_or_states,
        }
    }

    pub fn wait_until(self: *This, state_or_states: anytype, opts: anytype) !void {
        // implement a 750 us limit
        const time_limit = if (@hasField(@TypeOf(opts), "limit")) opts.limit else 750_000;
        const cycles = time_limit / 1000;
        var so_far: usize = 0;

        while (!check_against(self.get(), state_or_states)) : (so_far += 1) {
            if (so_far > cycles) return error.threadtooktoolong;
            std.Thread.sleep(1000);
        }
    }
};

pub threadlocal var this_thread: ?*anyopaque = null;
pub threadlocal var local_join_started: *bool = undefined;
pub threadlocal var self_pid: *const fn () beam.pid = undefined;

fn makes_error_result__(comptime F: type) bool {
    const NaiveReturnType = @typeInfo(F).@"fn".return_type.?;
    return (@typeInfo(NaiveReturnType) == .error_union);
}

pub fn Thread(comptime function: anytype) type {
    const F = @TypeOf(function);
    const Payload = beam.Payload(function);
    const Result = beam.WrappedResult(F);

    return struct {
        const This = @This();

        pid: beam.pid,
        env: beam.env,
        tid: beam.tid = undefined,
        refbin: e.ErlNifBinary = undefined,
        state: ThreadState = .prepped,
        join_started: bool = false,

        allocator: std.mem.Allocator,
        payload: Payload,
        result: ?*Result = null,

        pub fn launch(comptime ThreadResource: type, argc: c_int, args: [*c]const e.ErlNifTerm, payload_opts: anytype) !beam.term {
            // assign the context, as the self() function needs this to be correct.
            // note that opts MUST contain `payload_opts` field, which is a
            switch (beam.context.mode) {
                // allow these modes for future expansion.
                .synchronous, .dirty => {},
                else => @panic("threaded functions must be launched from synchronous, dirty_io, or dirty_cpu contexts."),
            }

            // thread struct necessities
            const allocator = options.allocator(.{});
            const thread_env = beam.alloc_env();

            // initialize the payload
            var error_index: u8 = undefined;
            const payload = beam.payload.build(function, argc, args, &error_index, payload_opts) catch {
                @panic("errors not implemented yet");
            };

            // initialize the thread struct
            // this needs to be allocator because it will be cleared by the
            // callback function, and the beam.allocator is undefined in that context.
            const threadptr = try beam.allocator.create(This);
            errdefer beam.allocator.destroy(threadptr);

            threadptr.* = .{ .env = thread_env, .pid = try beam.self(.{}), .payload = payload, .allocator = allocator, .result = try allocator.create(Result) };

            // build the resource and bind it to beam term.
            const resource = try ThreadResource.create(threadptr, .{});
            const res_term = beam.make(resource, .{});

            // copy the resource term into a binary so that we can resend it later.  We can't
            // directly do an env copy on this term, because that will cause it to have an
            // extra ownership count on it, and will prevent it from being destroyed and
            // we won't be able to trigger the resource destructor.
            threadptr.refbin = try beam.term_to_binary(res_term, .{});
            errdefer beam.release_binary(&threadptr.refbin);

            // set the self_pid function
            self_pid = &This.self_pid_fn;

            // launch the thread
            _ = e.enif_thread_create(name_ptr(), &threadptr.tid, wrapped, threadptr, null);

            threadptr.state.wait_until(.{ .running, .finished }, .{}) catch |err| {
                // TODO: do a thread exit operation here.
                return err;
            };

            return res_term;
        }

        // this is a wrapped function designed explicitly to be called by e.enif_thread_create.
        fn wrapped(void_thread: ?*anyopaque) callconv(.c) ?*anyopaque {
            const thread = @as(*This, @ptrCast(@alignCast(void_thread.?)));
            // set critical threadlocal variables
            local_join_started = &thread.join_started;

            beam.context = .{
                .mode = .threaded,
                .allocator = thread.allocator,
                .env = thread.env,
            };

            if (thread.state.exchange(.prepped, .running)) |state| switch (state) {
                .prepped => unreachable, // exchange can't return what it started with.
                .running => @panic("should not have reached running before executing thread"),
                .finished => @panic("should not have reached finished before executing thread"),
                .joining => @panic("should not have reached joining before executing thread"),
                .joined => @panic("should not have reached joined before executing thread"),
                .failed => @panic("should not have reached failed before executing thread"),
            };

            defer {
                // unpack the reference binary and send it to the parent process to signal
                // completion.  This stuff is "best effort", so errors can be discarded.
                // it's possible that the destination process has been killed.

                const bin = beam.binary_to_slice(thread.refbin);

                const SendResult = enum { @"error", done };

                const to_send: SendResult = if (thread.state.exchange(.running, .finished)) |state| switch (state) {
                    .failed => .@"error",
                    .running => unreachable,
                    .prepped => @panic("should not have regressed to prepped"),
                    .finished => @panic("should not have reached finished without executing thread"),
                    .joining => @panic("should not have reached joining without executing thread"),
                    .joined => @panic("should not have reached joined without executing thread"),
                } else .done;

                if (beam.binary_to_term(bin, .{})) |term| {
                    beam.send(thread.pid, .{ to_send, term }, .{}) catch {};
                } else |_| {}
            }

            this_thread = void_thread;

            if (Result == void) {
                @call(.auto, function, thread.payload);
                return null;
            } else {
                const result_ptr = thread.allocator.create(Result) catch {
                    thread.state.set(.failed);
                    return null;
                };

                if (comptime makes_error_result__(F)) {
                    if (@call(.auto, function, thread.payload)) |ok| {
                        result_ptr.* = .{ .ok = ok };
                    } else |err| {
                        // this is a thread error, sometimes the comptime semantics are unable to detect
                        // that error.processterminated is a possibility (because yield() might or might not
                        // be called in the function).  So we need to do a runtime check here.
                        // TODO: do better by having a comptime function that checks the return trace.
                        const TERMINATED = @intFromError(error.processterminated);
                        if (@intFromError(err) == TERMINATED) {
                            result_ptr.* = .{ .error_return_trace = beam.make_empty_list(.{}) };
                        } else {
                            const response = if (@import("builtin").os.tag == .windows)
                                .{ .@"error", err, null }
                            else
                                .{ .@"error", err, @errorReturnTrace() };
                            result_ptr.* = .{ .error_return_trace = beam.make(response, .{}) };
                        }
                    }

                    return result_ptr;
                } else {
                    result_ptr.* = @call(.auto, function, thread.payload);
                    return result_ptr;
                }
            }
        }

        const name = @typeName(F);
        fn name_ptr() [*c]u8 {
            // this needs to be done like this because enif_thread_create is
            // not const-correct.  In the future, we should actually fix this
            // by giving each thread a dynamic name, so that `name` can have
            // debug information attached.
            return @constCast(@as([*c]const u8, name));
        }

        pub fn get_info() *This {
            return @as(*This, @ptrCast(@alignCast(this_thread.?)));
        }

        pub fn makes_error_result() bool {
            return makes_error_result__(F);
        }

        fn self_pid_fn() beam.pid {
            return get_info().pid;
        }

        fn lock_join(self: *This) bool {
            _ = self;
            return true;
        }

        fn has_join_started(self: *This) bool {
            // true if the thread has gated through the join function.  false
            // if it is not, with the side effect of flipping marking it as
            // having gated.
            return @cmpxchgStrong(bool, &self.join_started, false, true, .monotonic, .monotonic) != null;
        }

        fn join_result(self: *This) !Result {
            if (Result == void) {
                return;
            }
            if (self.result) |result| {
                return result.*;
            } else {
                return error.threaderror;
            }
        }

        pub fn join(self: *This) !Result {
            // Mutex this over a boolean value so that only one thread can
            // perform the join at a time.  Other threads will be able to
            // obtain the result pointer, but they will need to wait for the
            // join to be completed by the first thread doing.  Note that this
            // boolean value will also be used by the thread loop to detect
            // terminations.

            if (self.has_join_started()) {
                // TODO: make this an async function with yields.
                try self.state.wait_until(.{ .joined, .failed }, .{});
                return self.join_result();
            }

            if (self.state.exchange(.finished, .joining)) |fail_state| {
                switch (fail_state) {
                    .running => {
                        // TODO: make this an async function with yields.
                        try self.state.wait_until(.finished, .{});
                    },
                    .failed => {
                        return error.threaderror;
                    },
                    .joined => @panic("should not have reached joined inside the protected join body"),
                    .joining => @panic("should not have reached joining inside the protected join body"),
                    .finished => unreachable, // this is the start state, so it can't be reached.
                    .prepped => @panic("should not have reached join before thread has started running"),
                }
            }

            defer self.state.set(.joined);

            // this is outside of the above if statement because we need to do both the
            // case where the thread had already finished itself.
            // in the case of a successful join operation, the result value will have been
            // loaded into the result pointer slot.
            var result_void: ?*anyopaque = undefined;

            if (e.enif_thread_join(self.tid, &result_void) == 0) {
                if (Result != void) {
                    self.result = @as(?*Result, @ptrCast(@alignCast(result_void)));
                }
                return self.join_result();
            } else {
                return error.processnotjoined;
            }
        }

        pub fn cleanup(self: *This) void {
            if (self.result) |result| {
                self.allocator.destroy(result);
            }
            beam.release_binary(&self.refbin);
            beam.free_env(self.env);

            // note that we allocated the thread pointer with allocator,
            // so we must destroy it with the same allocator.
            beam.allocator.destroy(self);
        }
    };
}

pub fn Callbacks(comptime ThreadType: type) type {
    return struct {
        pub fn dtor(dtor_ref: **ThreadType) void {
            const thread_ptr = dtor_ref.*;
            // join the thread at all costs, catch all failures, discard the result.
            // NB: this WILL cause a leak.
            _ = thread_ptr.join() catch return;
            thread_ptr.cleanup();
        }
    };
}

pub fn yield() !void {
    // to be called only from the yield module.
    if (@atomicLoad(bool, local_join_started, .monotonic)) {
        return error.processterminated;
    }
}
