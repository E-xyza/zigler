const std = @import("std");
const beam = @import("beam.zig");
const e = @import("erl_nif.zig");

const BeamThreadFn = *const fn (?*anyopaque) callconv(.C) ?*anyopaque;

const ThreadError = error{threaderror, threadtooktoolong};

pub const ThreadState = enum {
    const This = @This();
    prepped,  // harness has created the thread and is waiting for thread to check in.
    running,  // thread has checked in
    finished, // thread function has completed running
    joined,   // all activity on the thread has completed.

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

pub fn Thread(comptime Function: type) type {
    const name = @typeName(Function);
    comptime var Payload: type = undefined;
    comptime var Return: type = undefined;

    switch (@typeInfo(Function)) {
        .Fn => |F| {
            Return = F.return_type.?;
            Payload = F.args[0].arg_type.?;
        },
        else => @compileError("Expected a function type"),
    }

    const return_align = @alignOf(Return);
    _ = return_align;

    return struct {
        const This = @This();

        tid: beam.tid = undefined,
        pid: beam.pid = undefined,
        env: beam.env = undefined,
        ref: beam.term = undefined,
        payload: Payload,
        allocator: std.mem.Allocator,
        function: *const Function,
        state: ThreadState = .prepped,
        joining: bool = false,  // protected variable that should only be set by the first thread that tries to join.

        pub fn create(function: *const Function, payload: Payload, opts: anytype) !*This {
            const allocator = if (@hasField(@TypeOf(opts), "allocator")) opts.allocator else beam.allocator;
            const thread = try allocator.create(This);

            thread.* = .{
                .env = beam.alloc_env(),
                .payload = payload,
                .allocator = allocator,
                .function = function,
            };

            return thread;
        }

        pub fn start(self: *This, env: beam.env, comptime Resource: type) !beam.term {
            self.pid = beam.self(env) catch @panic("threads must be started from a process");
            self.env = beam.alloc_env();

            const resource = try Resource.create(self, .{});

            const parent_ref = beam.make(env, resource, .{ .output_type = .default });
            self.ref = beam.copy(self.env, parent_ref);

            if (e.enif_thread_create(name_ptr(), &self.tid, wrapped, self, null) != 0) {
                return error.threaderror;
            }

            resource.release(); // this is ok since we have already made it, above.

            return parent_ref;
        }

        fn wrapped(void_thread: ?*anyopaque) callconv(.C) ?*anyopaque {
            const thread = @ptrCast(*This, @alignCast(@alignOf(This), void_thread.?));
            // set the threadlocal joining
            thread_joining = &thread.joining;

            if (thread.state.exchange(.prepped, .running)) |state| switch (state) {
                .prepped => unreachable, // exchange can't return what it started with.
                .running => @panic("should not have reached running before executing thread"),
                .finished => @panic("should not have reached finished before executing thread"),
                .joined => @panic("should not have reached joined before executing thread"),
            };

            defer {
                if (thread.state.exchange(.running, .finished)) |state| switch (state) {
                    .prepped => @panic("should not have regressed to prepped"), 
                    .running => unreachable,
                    .finished => @panic("should not have reached finished without executing thread"),
                    .joined => @panic("should not have reached joined without executing thread"),
                };
            }

            beam.context = .threaded;
            this_thread = void_thread;

            if (Return == void) {
                thread.function(thread.payload);
                // ignore the failed send instruction if the process has been killed.
                _ = beam.send(thread.env, thread.pid, .{ .done, thread.ref }) catch {};
                return null;
            } else {
                const result = thread.function(thread.payload);
                //thread.retpointer.* = result;
                // ignore the failed send instruction if the process has been killed.
                _ = beam.send(thread.env, thread.pid, .{ .done, thread.ref, result }) catch {};
                //return thread.retpointer;
                return null;
            }
        }

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
            // returns true if we succeded in obtaining the lock.  False otherwise.
            return @cmpxchgStrong(bool, &self.joining, false, true, .Monotonic, .Monotonic) == null;
        }

        pub fn is_joining(self: *This) bool {
            return @atomicLoad(bool, &self.joining, .Monotonic);
        }

        pub fn join(self: *This, opts: anytype) !void {
            // be the first thread to obtain a lock on the process.
            if (self.lock_join()) {
                self.state.wait_until(.finished, opts) catch |err| {
                    std.log.warn("a thread failed to join because it took too long\n", .{});
                    return err;
                };
                _ = e.enif_thread_join(self.tid, null);
                self.state.set(.joined);
            } else {
                // someone else has already joined.
                self.state.wait_until(.joined, opts) catch |err| {
                    std.log.warn("a thread failed to join because it took too long\n", .{});
                    return err;
                };
            }

            // TODO: return the working value.
            return;
        }

        pub fn cleanup(self: *This) void {
            beam.free_env(self.env);
            //self.allocator.destroy(self.retpointer);
            self.allocator.destroy(self);
        }
    };
}

pub fn ThreadCallbacks(comptime ThreadType: type) type {
    return struct {
        pub fn dtor(_: beam.env, thread_ptr: **ThreadType) void {
            const thread = thread_ptr.*;
            // always (at least attempt to) perform a thread join.
            _ = thread.join(.{}) catch { return; };
            // add some time?
            //thread.cleanup();
        }
    };
}
