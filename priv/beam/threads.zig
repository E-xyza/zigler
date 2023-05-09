const std = @import("std");
const beam = @import("beam.zig");
const e = @import("erl_nif.zig");

const BeamThreadFn = *const fn (?*anyopaque) callconv(.C) ?*anyopaque;

const ThreadError = error{threaderror};

pub const ThreadState = enum { prepped, launched, done, joined };

pub threadlocal var this_thread: ?*anyopaque = null;
pub threadlocal var thread_state: ?*ThreadState = null;

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

    return struct {
        const This = @This();

        tid: beam.tid,
        pid: beam.pid,
        env: beam.env,
        ref: beam.term,
        retpointer: *Return,
        payload: Payload,
        allocator: std.mem.Allocator,
        function: *const Function,
        state: ThreadState,

        fn wrapped(void_thread: ?*anyopaque) callconv(.C) ?*anyopaque {
            const thread = @ptrCast(*This, @alignCast(@alignOf(This), void_thread.?));
            thread_state = &thread.state;
            thread.state = .launched;
            defer thread.state = .done;

            beam.context = .threaded;
            this_thread = void_thread;

            if (Return == void) {
                thread.function(thread.payload);
                // ignore the failed send instruction if the process has been killed.
                _ = beam.send(thread.env, thread.pid, .{ .done, thread.ref }) catch {};
                return null;
            } else {
                const result = thread.function(thread.payload);
                thread.retpointer.* = result;
                // ignore the failed send instruction if the process has been killed.
                _ = beam.send(thread.env, thread.pid, .{ .done, thread.ref }) catch {};
                return thread.retpointer;
            }
        }

        pub fn prep(function: *const Function, payload: Payload, opts: anytype) !*This {
            const allocator = if (@hasField(@TypeOf(opts), "allocator")) opts.allocator else beam.allocator;
            const thread = try allocator.create(This);
            errdefer allocator.destroy(thread);

            thread.env = beam.alloc_env();
            errdefer beam.free_env(thread.env);

            thread.retpointer = try allocator.create(Return);
            thread.allocator = allocator;
            thread.function = function;
            thread.payload = payload;
            thread.state = .prepped;

            return thread;
        }

        fn name_ptr() [*c]u8 {
            // this needs to be done like this because enif_thread_create is
            // not const-correct.  In the future, we should actually fix this
            // by giving each thread a dynamic name, so that `name` can have
            // debug information attached.
            return @intToPtr([*c]u8, @ptrToInt(&name));
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

        pub fn get_info() *This {
            return @ptrCast(*This, @alignCast(@alignOf(This), this_thread.?));
        }

        pub fn join(self: *This) !Return {

                defer self.state = .joined;

                var retval: ?*anyopaque = null;

                if (e.enif_thread_join(self.tid, &retval) != 0) {
                    return error.threadjoinerror;
                }

                if (Return != void) {
                    self.retpointer = @ptrCast(*Return, @alignCast(return_align, retval.?));
                    return self.retpointer.*;
                }
        }

        pub fn cleanup(self: *This) void {
            beam.free_env(self.env);
            self.allocator.destroy(self.retpointer);
            self.allocator.destroy(self);
        }
    };
}

pub fn ThreadCallbacks(comptime ThreadType: type) type {
    return struct {
        pub fn dtor(env: beam.env, thread_ptr: **ThreadType) void {
            _ = env;
            const thread = thread_ptr.*;

            while (thread.state == .prepped) {
                std.time.sleep(1000);
            }

            // only launched, done, and joined are available here
            if (thread.state == .launched) {
                thread.state = .done;
            }

            if (thread.state != .joined) {
               _ = thread.join() catch {};
            }

            thread.cleanup();
        }
    };
}
