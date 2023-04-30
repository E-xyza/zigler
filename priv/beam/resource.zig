const std = @import("std");
const beam = @import("beam.zig");
const e = @import("erl_nif.zig").e;

const builtin = std.builtin;
const Allocator = std.mem.Allocator;
const MAX_ALIGN = @import("allocator.zig").MAX_ALIGN;

const ResourceMode = enum { reference, binary };

const ResourceOpts = struct {
    mode: ResourceMode = .reference,
    Callbacks: ?type = null,
};

pub fn Resource(comptime T: type, comptime root: type, comptime opts: ResourceOpts) type {
    return struct {
        __payload: *T,

        /// initialization function, which should be called at runtime by the module's init function.  The
        /// primary job of this function is to assign the resource type generating function to
        /// the root namespace.
        pub fn init(comptime module: []const u8, env: beam.env) *e.ErlNifResourceType {
            // load em up.  Note all callbacks are optional and may be set to null.
            // see: https://www.erlang.org/doc/man/erl_nif.html#enif_init_resource_type
            var init_struct: e.ErlNifResourceTypeInit = .{ .dtor = null, .stop = null, .down = null, .dyncall = null, .members = 0 };
            if (opts.Callbacks) |Callbacks| {
                const Shimmed = MakeShimmed(Callbacks);

                if (Callbacks.dtor) |_| {
                    init_struct.dtor = Shimmed.dtor;
                    init_struct.members = 1;
                }

                if (Callbacks.stop) |_| {
                    init_struct.stop = Shimmed.stop;
                    init_struct.members = 2;
                }

                if (Callbacks.down) |_| {
                    init_struct.down = Shimmed.down;
                    init_struct.members = 3;
                }

                if (Callbacks.dyncall) |_| {
                    init_struct.dyncall = Shimmed.dyncall;
                    init_struct.members = 4;
                }
            }

            return if (e.enif_init_resource_type(env, @typeName(T) ++ "-" ++ module, &init_struct, e.ERL_NIF_RT_CREATE, null)) |resource_type| resource_type else @panic("couldn't initialize the resource type for" ++ @typeName(T));
        }

        pub fn create(data: T) !@This() {
            var allocator = Allocator{ .ptr = undefined, .vtable = &resource_vtable };

            if (! beam.is_sema) {
                root.set_resource(T, @ptrCast(**e.ErlNifResourceType, &allocator.ptr));
            }

            if (@sizeOf(@TypeOf(data)) == 0) {
                @compileError("you cannot create a resource for a zero-byte object");
            }

            const resource_payload = try allocator.create(T);
            resource_payload.* = data;
            return .{ .__payload = resource_payload };
        }

        pub fn unpack(self: @This()) T {
            return self.__payload.*;
        }

        pub fn update(self: @This(), data: T) void {
            self.__payload.* = data;
        }
    };
}

pub fn MaybeUnwrap(comptime s: builtin.Type.Struct) ?type {
    // verify that this is indeed a resource.  A resource has the
    // a single field called `__payload` which is a pointer to the wrapped type.
    // return the wrapped type, otherwise return null.

    if (s.fields.len != 1) return null;
    if (!std.mem.eql(u8, s.fields[0].name, "__payload")) return null;

    switch (@typeInfo(s.fields[0].field_type)) {
        .Pointer => |p| {
            if (p.size != .One) return null;
            if (p.is_allowzero) return null;
            return p.child;
        },
        else => return null,
    }
}

fn MakeShimmed(comptime BeamCallbacks: ?type, T: type) type {
    return struct {
        fn dtor(env: beam.env, obj: ?*anyopaque) callconv(.C) void {
            if (@hasDecl(BeamCallbacks, "dtor")){
                const typed_object_ptr = @ptrCast(*T, obj.?);
                BeamCallbacks.dtor(env, typed_object_ptr);
            }
        }

        fn down(env: beam.env, obj: ?*anyopaque, pid: ?*beam.pid, monitor: ?*beam.monitor) callconv(.C) void {
            if (@hasDecl(BeamCallbacks, "down")){
                const typed_object_ptr = @ptrCast(*T, obj.?);
                BeamCallbacks.down(env, typed_object_ptr, pid.?.*, monitor.?.*);
            }
        }

        fn stop(env: beam.env, obj: ?*anyopaque, event: beam.event, is_direct_call: c_int) callconv(.C) void {
            if (@hasDecl(BeamCallbacks, "stop")){
                const typed_object_ptr = @ptrCast(*T, obj.?);
                BeamCallbacks.stop(env, typed_object_ptr, event, is_direct_call != 0);
            }
        }

        fn dyncall(env: beam.env, obj: ?*anyopaque, calldata: ?*anyopaque) callconv(.C) void {
            if (@hasDecl(BeamCallbacks, "dyncall")){
                const typed_object_ptr = @ptrCast(*T, obj.?);
                return BeamCallbacks.dyncall(env, typed_object_ptr, calldata);
            }
        }
    };
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