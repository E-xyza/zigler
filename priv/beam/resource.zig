const std = @import("std");
const beam = @import("beam.zig");
const e = @import("erl_nif");
const options = @import("options.zig");

const builtin = std.builtin;
const Allocator = std.mem.Allocator;
const MAX_ALIGN = @import("allocator.zig").MAX_ALIGN;

const ResourceOpts = struct {
    Callbacks: ?type = null,
};

const CreateOpts = struct {
    released: bool = true,
};

const ResourceError = error{incorrect_resource_type};

pub fn Resource(comptime T: type, comptime root: type, comptime opts: ResourceOpts) type {
    return struct {
        pub const __is_zigler_resource = true;
        
        __payload: *T,

        // indicates if the resource, in context of this nif call, should be released at the end
        // of the nif call.  This is set in the `create_opts` parameter of the `create` function.
        __should_release: bool = true,

        /// initialization function, which should be called at runtime by the module's init function.  The
        /// primary job of this function is to assign the resource type generating function to
        /// the root namespace.
        pub fn init(comptime module: []const u8, init_opts: anytype) *e.ErlNifResourceType {
            // load em up.  Note all callbacks are optional and may be set to null.
            // see: https://www.erlang.org/doc/man/erl_nif.html#enif_init_resource_type
            var init_struct = e.ErlNifResourceTypeInit{ .dtor = null, .stop = null, .down = null, .dyncall = null, .members = 0 };

            if (opts.Callbacks) |Callbacks| {
                const Shimmed = MakeShimmed(Callbacks);

                if (@hasDecl(Callbacks, "dtor")) {
                    assert_type_matches(@TypeOf(Callbacks.dtor), fn (*T) void);
                    init_struct.dtor = Shimmed.dtor;
                    init_struct.members += 1;
                }

                if (@hasDecl(Callbacks, "stop")) {
                    assert_type_matches(@TypeOf(Callbacks.stop), fn (*T, beam.pid, beam.monitor) void);
                    init_struct.stop = Shimmed.stop;
                    init_struct.members += 1;
                }

                if (@hasDecl(Callbacks, "down")) {
                    assert_type_matches(@TypeOf(Callbacks.down), fn (*T, beam.event, bool) void);
                    init_struct.down = Shimmed.down;
                    init_struct.members += 1;
                }

                if (@hasDecl(Callbacks, "dyncall")) {
                    assert_type_matches(@TypeOf(Callbacks.dyncall), fn (*T, ?*anyopaque) void);
                    init_struct.dyncall = Shimmed.dyncall;
                    init_struct.members += 1;
                }
            }

            return e.enif_init_resource_type(options.env(init_opts), @typeName(T) ++ "-" ++ module, &init_struct, e.ERL_NIF_RT_CREATE, null).?;
        }

        pub fn resource_type(_: @This()) *e.ErlNifResourceType {
            beam.ignore_when_sema();

            var resource_type_struct: *e.ErlNifResourceType = undefined;
            root.set_resource(@This(), &resource_type_struct);
            return resource_type_struct;
        }

        pub fn create(data: T, create_opts: CreateOpts) !@This() {
            beam.ignore_when_sema();

            if (@sizeOf(T) == 0) {
                @compileError("you cannot create a resource for a zero-byte object");
            }

            var allocator = Allocator{ .ptr = undefined, .vtable = &resource_vtable };

            root.set_resource(@This(), @ptrCast(&allocator.ptr));

            const resource_payload = try allocator.create(T);
            resource_payload.* = data;

            return .{ .__payload = resource_payload, .__should_release = create_opts.released };
        }

        pub fn maybe_release(self: @This()) void {
            if (self.__should_release) {
                self.release();
            }
        }

        pub fn release(self: @This()) void {
            beam.ignore_when_sema();
            e.enif_release_resource(@ptrCast(self.__payload));
        }

        pub fn keep(self: @This()) void {
            beam.ignore_when_sema();
            _ = e.enif_keep_resource(@ptrCast(self.__payload));
        }

        pub fn unpack(self: @This()) T {
            return self.__payload.*;
        }

        pub fn update(self: @This(), data: T) void {
            self.__payload.* = data;
        }

        // get and make functions should only be called internally, by get.zig
        // and make.zig modules.  They are not intended to be called directly
        // by end user.

        pub fn get(self: *@This(), src: beam.term, get_opts: anytype) !void {
            if (@hasField(@TypeOf(get_opts), "released")) {
                self.__should_release = get_opts.released;
            }

            const resource_target = @as(?*?*anyopaque, @ptrCast(&self.__payload));
            if (e.enif_get_resource(options.env(get_opts), src.v, self.resource_type(), resource_target) == 0)
                return error.incorrect_resource_type;
        }

        pub fn make(self: @This(), make_opts: anytype) beam.term {
            const as = options.output(make_opts);
            defer self.maybe_release();

            return switch (as) {
                .default => .{ .v = e.enif_make_resource(options.env(make_opts), @ptrCast(self.__payload)) },
                else => @compileError("resource currently only supports default output"),
            };
        }

        fn default_encoder(payload: *const T) []const u8 {
            return switch (T) {
                []const u8 => payload.*,
                []u8 => payload.*,
                else => @panic("a resource only has default encoder for strings"),
            };
        }

        fn MakeShimmed(comptime Callbacks: type) type {
            return struct {
                fn to_typed(obj: ?*anyopaque) *T {
                    return @ptrCast(@alignCast(obj.?));
                }

                fn dtor(env_: beam.env, obj: ?*anyopaque) callconv(.c) void {
                    set_callback_context(env_);

                    if (@hasDecl(Callbacks, "dtor")) {
                        Callbacks.dtor(to_typed(obj));
                    }
                }

                fn down(env_: beam.env, obj: ?*anyopaque, pid: [*c]beam.pid, monitor: [*c]beam.monitor) callconv(.c) void {
                    set_callback_context(env_);

                    if (@hasDecl(Callbacks, "down")) {
                        Callbacks.down(to_typed(obj), pid[0], monitor[0]);
                    }
                }

                fn stop(env_: beam.env, obj: ?*anyopaque, event: beam.event, is_direct_call: c_int) callconv(.c) void {
                    set_callback_context(env_);

                    if (@hasDecl(Callbacks, "stop")) {
                        Callbacks.stop(to_typed(obj), event, is_direct_call != 0);
                    }
                }

                fn dyncall(env_: beam.env, obj: ?*anyopaque, calldata: ?*anyopaque) callconv(.c) void {
                    set_callback_context(env_);

                    if (@hasDecl(Callbacks, "dyncall")) {
                        return Callbacks.dyncall(to_typed(obj), calldata);
                    }
                }
            };
        }
    };
}

fn set_callback_context(env_: beam.env) void {
    beam.context = .{
        .mode = .callback,
        .env = env_,
        .allocator = beam.allocator,
    };
}

pub fn MaybeUnwrap(comptime s: builtin.Type.Struct) ?type {
    // verify that this is indeed a resource.  A resource has the
    // a single field called `__payload` which is a pointer to the wrapped type.
    // return the wrapped type, otherwise return null.

    if (s.fields.len != 2) return null;
    if (!std.mem.eql(u8, s.fields[0].name, "__payload")) return null;
    if (!std.mem.eql(u8, s.fields[1].name, "__should_release")) return null;

    switch (@typeInfo(s.fields[0].type)) {
        .pointer => |p| {
            if (p.size != .one) return null;
            if (p.is_allowzero) return null;
            return p.child;
        },
        else => return null,
    }
}

fn resource_alloc(
    resource_ptr: *anyopaque,
    len: usize,
    ptr_align: std.mem.Alignment,
    _: usize,
) ?[*]u8 {
    if (ptr_align.compare(.gt, comptime .fromByteUnits(MAX_ALIGN))) return null;
    // don't deal with alignment issues at the moment.
    const resource_type = @as(*e.ErlNifResourceType, @ptrCast(resource_ptr));
    const ptr = e.enif_alloc_resource(resource_type, @as(c_uint, @intCast(len))) orelse return null;
    return @ptrCast(ptr);
}

const resource_vtable = Allocator.VTable{
    .alloc = resource_alloc,
    .resize = std.mem.Allocator.noResize,
    .remap = std.mem.Allocator.noRemap,
    .free = std.mem.Allocator.noFree,
};

fn assert_type_matches(comptime t1: type, comptime t2: type) void {
    if (t1 != t2) {
        @compileError("type " ++ @typeName(t1) ++ " doesn't match expected " ++ @typeName(t2));
    }
}
