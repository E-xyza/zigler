const std = @import("std");
const beam = @import("beam.zig");
const e = @import("erl_nif.zig");

const BeamThreadFn = *const fn (?*anyopaque) callconv(.C) ?*anyopaque;

const ThreadError = error{threaderror};

pub threadlocal var thread_info: ?*anyopaque = null;

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
        env: beam.env, // needs to be freed
        ref: beam.term,
        retval: *Return, // needs to be freed
        payload: Payload,
        allocator: std.mem.Allocator,
        function: *const Function,

        fn wrapped(c_info: ?*anyopaque) callconv(.C) ?*anyopaque {
            beam.context = .threaded;
            thread_info = c_info;
            const info = @ptrCast(*This, @alignCast(@alignOf(This), c_info.?));

            if (Return == void) {
                info.function(info.payload);
                _ = beam.send(info.env, info.pid, .{ .done, info.ref }) catch @panic("process died before thread");
                return null;
            } else {
                info.retval.* = info.function(info.payload);
                _ = beam.send(info.env, info.pid, .{ .done, info.ref }) catch @panic("process died before thread");
                return info.retval;
            }
        }

        pub fn prep(function: *const Function, payload: Payload, opts: anytype) !This {
            var info: This = undefined;
            errdefer beam.free_env(info.env);

            if (@hasField(@TypeOf(opts), "allocator")) {
                info.allocator = opts.allocator;
            } else {
                info.allocator = beam.allocator;
            }

            if (Return != void) {
                info.retval = try info.allocator.create(Return);
            }

            info.function = function;
            info.payload = payload;

            return info;
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

            const resource = try Resource.create(self.*, .{});
            const parent_ref = beam.make(env, resource, .{ .output_type = .default });
            self.ref = beam.copy(self.env, parent_ref);

            if (e.enif_thread_create(name_ptr(), &self.tid, wrapped, self, null) != 0) {
                return error.threaderror;
            }

            resource.update(self.*);

            return parent_ref;
        }

        pub fn get_thread_info() *This {
            return @ptrCast(*This, @alignCast(@alignOf(This), thread_info.?));
        }

        pub fn join(self: This) !Return {
            var retval: ?*anyopaque = null;
            defer if (Return != void) {
                self.allocator.destroy(self.retval);
            };

            if (e.enif_thread_join(self.tid, &retval) != 0) {
                return error.threadjoinerror;
            }

            if (Return != void) {
                const result_ptr = @ptrCast(*Return, @alignCast(return_align, retval.?));
                return result_ptr.*;
            }
        }
    };
}
