const std = @import("std");
const beam = @import("beam.zig");
const e = @import("erl_nif.zig").e;

const builtin = std.builtin;
const Allocator = std.mem.Allocator;
const MAX_ALIGN = @import("allocator.zig").MAX_ALIGN;

const ResourceOpts = struct {
    Callbacks: ?type = null,
};

const CreateOpts = struct {
    released: bool = true,
};

const OutputType = enum {
    default,
    binary,

    pub fn select(opts: anytype) OutputType {
        if (@hasField(@TypeOf(opts), "output_type")) {
            return opts.output_type;
        } else {
            return .default;
        }
    }
};

pub fn Resource(comptime T: type, comptime root: type, comptime opts: ResourceOpts) type {
    return struct {
        __payload: *T,

        // indicates if the resource, in context of this nif call, should be released at the end
        // of the nif call.  This is set in the `create_opts` parameter of the `create` function.
        __should_release: bool = true,

        /// initialization function, which should be called at runtime by the module's init function.  The
        /// primary job of this function is to assign the resource type generating function to
        /// the root namespace.
        pub fn init(comptime module: []const u8, env: beam.env) *e.ErlNifResourceType {
            // load em up.  Note all callbacks are optional and may be set to null.
            // see: https://www.erlang.org/doc/man/erl_nif.html#enif_init_resource_type
            var init_struct = e.ErlNifResourceTypeInit{ .dtor = null, .stop = null, .down = null, .dyncall = null, .members = 0 };

            if (opts.Callbacks) |Callbacks| {
                const Shimmed = MakeShimmed(Callbacks);

                if (@hasDecl(Callbacks, "dtor")) {
                    assert_type_matches(@TypeOf(Callbacks.dtor), fn (beam.env, *T) void);
                    init_struct.dtor = Shimmed.dtor;
                    init_struct.members += 1;
                }

                if (@hasDecl(Callbacks, "stop")) {
                    assert_type_matches(@TypeOf(Callbacks.stop), fn (beam.env, *T, beam.pid, beam.monitor) void);
                    init_struct.stop = Shimmed.stop;
                    init_struct.members += 1;
                }

                if (@hasDecl(Callbacks, "down")) {
                    assert_type_matches(@TypeOf(Callbacks.down), fn (beam.env, *T, beam.event, bool) void);
                    init_struct.down = Shimmed.down;
                    init_struct.members += 1;
                }

                if (@hasDecl(Callbacks, "dyncall")) {
                    assert_type_matches(@TypeOf(Callbacks.dyncall), fn (beam.env, *T, ?*anyopaque) void);
                    init_struct.dyncall = Shimmed.dyncall;
                    init_struct.members += 1;
                }
            }

            return e.enif_init_resource_type(env, @typeName(T) ++ "-" ++ module, &init_struct, e.ERL_NIF_RT_CREATE, null).?;
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

            root.set_resource(@This(), @ptrCast(**e.ErlNifResourceType, &allocator.ptr));

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
            e.enif_release_resource(@ptrCast(*anyopaque, self.__payload));
        }

        pub fn keep(self: @This()) void {
            beam.ignore_when_sema();
            _ = e.enif_keep_resource(@ptrCast(*anyopaque, self.__payload));
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

        pub fn get(self: *@This(), env: beam.env, src: beam.term, get_opts: anytype) c_int {
            if (@hasField(@TypeOf(get_opts), "released")) {
                self.__should_release = get_opts.released;
            }

            const resource_target = @ptrCast(?*?*anyopaque, &self.__payload);
            return e.enif_get_resource(env, src.v, self.resource_type(), resource_target);
        }

        pub fn make(self: @This(), env: beam.env, comptime make_opts: anytype) beam.term {
            const output_type = comptime OutputType.select(make_opts);
            defer self.maybe_release();
            switch (output_type) {
                .default => {

                    return .{ .v = e.enif_make_resource(env, @ptrCast(*anyopaque, self.__payload)) };
                },
                .binary => {
                    const encoder = if (@hasField(@TypeOf(make_opts), "encoder")) make_opts.encoder else default_encoder;
                    assert_type_matches(@TypeOf(encoder), fn (*const T) []const u8);
                    const bytes: []const u8 = encoder(self.__payload);
                    return .{ .v = e.enif_make_resource_binary(env, @ptrCast(*anyopaque, self.__payload), bytes.ptr, bytes.len) };
                },
            }
        }

        fn default_encoder(payload: *const T) []const u8 {
            return switch (T) {
                []const u8 => payload.*,
                []u8 => payload.*,
                else => @compileError("a resource only has default encoder for strings"),
            };
        }

        fn MakeShimmed(comptime Callbacks: type) type {
            return struct {
                fn to_typed(obj: ?*anyopaque) *T {
                    return @ptrCast(*T, @alignCast(@alignOf(T), obj.?));
                }

                fn dtor(env: beam.env, obj: ?*anyopaque) callconv(.C) void {
                    beam.context = .callback;
                    if (@hasDecl(Callbacks, "dtor")) {
                        Callbacks.dtor(env, to_typed(obj));
                    }
                }

                fn down(env: beam.env, obj: ?*anyopaque, pid: [*c]beam.pid, monitor: [*c]beam.monitor) callconv(.C) void {
                    beam.context = .callback;
                    if (@hasDecl(Callbacks, "down")) {
                        Callbacks.down(env, to_typed(obj), pid[0], monitor[0]);
                    }
                }

                fn stop(env: beam.env, obj: ?*anyopaque, event: beam.event, is_direct_call: c_int) callconv(.C) void {
                    beam.context = .callback;
                    if (@hasDecl(Callbacks, "stop")) {
                        Callbacks.stop(env, to_typed(obj), event, is_direct_call != 0);
                    }
                }

                fn dyncall(env: beam.env, obj: ?*anyopaque, calldata: ?*anyopaque) callconv(.C) void {
                    beam.context = .callback;
                    if (@hasDecl(Callbacks, "dyncall")) {
                        return Callbacks.dyncall(env, to_typed(obj), calldata);
                    }
                }
            };
        }
    };
}

pub fn MaybeUnwrap(comptime s: builtin.Type.Struct) ?type {
    // verify that this is indeed a resource.  A resource has the
    // a single field called `__payload` which is a pointer to the wrapped type.
    // return the wrapped type, otherwise return null.

    if (s.fields.len != 2) return null;
    if (!std.mem.eql(u8, s.fields[0].name, "__payload")) return null;
    if (!std.mem.eql(u8, s.fields[1].name, "__should_release")) return null;

    switch (@typeInfo(s.fields[0].field_type)) {
        .Pointer => |p| {
            if (p.size != .One) return null;
            if (p.is_allowzero) return null;
            return p.child;
        },
        else => return null,
    }
}

fn resource_alloc(
    resource_ptr: *anyopaque,
    len: usize,
    ptr_align: u29,
    _: u29,
    _: usize,
) Allocator.Error![]u8 {
    if (ptr_align > MAX_ALIGN) {
        return error.OutOfMemory;
    }
    // don't deal with alignment issues at the moment.
    const resource_type = @ptrCast(*e.ErlNifResourceType, resource_ptr);
    const ptr = e.enif_alloc_resource(resource_type, @intCast(c_uint, len)) orelse return error.OutOfMemory;
    return @ptrCast([*]u8, ptr)[0..len];
}

fn noresize(_: *anyopaque, _: []u8, _: u29, _: usize, _: u29, _: usize) ?usize {
    return null;
}

fn nofree(_: *anyopaque, _: []u8, _: u29, _: usize) void {}

const resource_vtable = Allocator.VTable{
    .alloc = resource_alloc,
    .resize = noresize,
    .free = nofree,
};

fn assert_type_matches(comptime t1: type, comptime t2: type) void {
    if (t1 != t2) {
        @compileError("type " ++ @typeName(t1) ++ " doesn't match expected " ++ @typeName(t2));
    }
}
