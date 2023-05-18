const std = @import("std");
const beam = @import("beam.zig");
const e = @import("erl_nif.zig");

const BeamThreadFn = *const fn (?*anyopaque) callconv(.C) ?*anyopaque;

const ThreadError = error{ threaderror, threadtooktoolong, processterminated };

pub const ThreadState = enum {
    const This = @This();
    prepped, // harness has created the thread and is waiting for thread to check in.
    running, // thread has checked in
    finished, // thread function has completed running
    joined, // all activity on the thread has completed.

    pub fn set(self: *This, state: ThreadState) void {
        @atomicStore(This, self, state, .Monotonic);
    }

    pub fn get(self: *This) ThreadState {
        return @atomicLoad(This, self, .Monotonic);
    }

    pub fn exchange(self: *This, old: ThreadState, new: ThreadState) ?ThreadState {
        return @cmpxchgStrong(This, self, old, new, .Monotonic, .Monotonic);
    }

    pub fn wait_while(self: *This, state: ThreadState) void {
        while (self.get() == state) {
            std.time.sleep(1000);
        }
    }

    pub fn wait_until(self: *This, state: ThreadState, opts: anytype) !void {
        // implement a 50 ms limit
        const time_limit = if (@hasField(@TypeOf(opts), "limit")) opts.limit else 50_000;
        const cycles = time_limit / 1000;
        var so_far: usize = 0;
        while (self.get() != state) : (so_far += 1) {
            if (so_far > cycles) return error.threadtooktoolong;
            std.time.sleep(1000);
        }
    }
};

pub threadlocal var this_thread: ?*anyopaque = null;
pub threadlocal var thread_joining: *bool = undefined;

pub fn Thread(comptime function: anytype) type {
    const F = @TypeOf(function);
    comptime var Payload = beam.Payload(function);
    comptime var Return = @typeInfo(F).Fn.return_type.?;

    const return_align = @alignOf(Return);
    _ = return_align;

    return struct {
        const This = @This();

        tid: beam.tid = undefined,
        pid: beam.pid = undefined,
        env: beam.env = undefined,
        ref: beam.term = undefined,

        pub fn launch(comptime ThreadResource: type, env: beam.env, args: Payload) !beam.term {
            _ = args;
            _ = ThreadResource;
            return beam.make(env, .ok, .{});
        }

        // this is a wrapped function designed explicitly to be called by e.enif_thread_create.
        fn wrapped(void_thread: ?*anyopaque) callconv(.C) ?*anyopaque {
            const thread = @ptrCast(*This, @alignCast(@alignOf(This), void_thread.?));
            // set critical threadlocal variables
            thread_joining = &thread.joining;
            beam.allocator = &thread.allocator;
            beam.context = .threaded;

            if (thread.state.exchange(.prepped, .running)) |state| switch (state) {
                .prepped => unreachable, // exchange can't return what it started with.
                .running => @panic("should not have reached running before executing thread"),
                .finished => @panic("should not have reached finished before executing thread"),
                .joined => @panic("should not have reached joined before executing thread"),
            };

            defer {
                if (thread.state.exchange(.running, .finished)) |state| switch (state) {
                    .running => unreachable,
                    .prepped => @panic("should not have regressed to prepped"),
                    .finished => @panic("should not have reached finished without executing thread"),
                    .joined => @panic("should not have reached joined without executing thread"),
                };
            }

            this_thread = void_thread;

            if (Return == void) {
                thread.function(thread.payload);
                // ignore the failed send instruction if the process has been killed.
                //_ = beam.send(thread.env, thread.pid, .{ .done, thread.ref }) catch {};
                return null;
            } else {
                const result = thread.function(thread.payload);
                thread.retptr.* = result;
                // ignore the failed send instruction if the process has been killed.
                //_ = beam.send(thread.env, thread.pid, .{ .done, thread.ref, result }) catch {};
                //return thread.retpointer;

                return thread.retptr;
            }
        }

        const name = @typeName(F);
        fn name_ptr() [*c]u8 {
            // this needs to be done like this because enif_thread_create is
            // not const-correct.  In the future, we should actually fix this
            // by giving each thread a dynamic name, so that `name` can have
            // debug information attached.
            return @intToPtr([*c]u8, @ptrToInt(&name));
        }

        pub fn get_info() *This {
            return @ptrCast(*This, @alignCast(@alignOf(This), this_thread.?));
        }

        fn lock_join(self: *This) bool {
            _ = self;
            return true;
        }

        pub fn is_joining(self: *This) bool {
            return @atomicLoad(bool, &self.joining, .Monotonic);
        }

        pub fn join(self: *This, comptime opts: anytype) !void {
            _ = opts;
            _ = self;
            return;
        }

        pub fn cleanup(self: *This) void {
            _ = self;
        }
    };
}

pub fn ThreadCallbacks(comptime ThreadType: type) type {
    return struct {
        pub fn dtor(_: beam.env, thread_ptr: **ThreadType) void {
            _ = thread_ptr;
        }
    };
}

fn is_thread_joining() bool {
    return @atomicLoad(bool, thread_joining, .Monotonic);
}

pub fn yield() !void {
    return error.processterminated;
}
