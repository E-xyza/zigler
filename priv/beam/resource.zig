const std = @import("std");
const beam = @import("beam.zig");
const e = @import("erl_nif.zig").e;

const builtin = std.builtin;
const Allocator = std.mem.Allocator;
const MAX_ALIGN = @import("allocator.zig").MAX_ALIGN;

pub fn Resource(comptime T: type) type {
    return struct {
        // we put this function in here to make sure it's got something special going on
        // and that it's not just a regular struct
        pub fn __wraps() type {
            return T;
        }
        resource_payload: *T,
    };
}

pub fn MaybeUnwrap(comptime s: builtin.Type.Struct) ?type {
    // verify that this is indeed a resource.  A resource has the
    // `__wraps` function and a single field called `resource_payload`
    // which is a pointer to the wrapped type.

    if (s.decls.len != 1) return null;
    if (!std.mem.eql(u8, s.decls[0].name, "__wraps")) return null;
    if (s.fields.len != 1) return null;
    if (!std.mem.eql(u8, s.fields[0].name, "resource_payload")) return null;

    switch (@typeInfo(s.fields[0].field_type)) {
        .Pointer => |p| {
            if (p.size != .One) return null;
            if (p.is_allowzero) return null;
            return p.child;
        },
        else => return null,
    }
}

pub fn Unwrap(comptime T: type) type {
    switch (@typeInfo(T)) {
        .Struct => |s| {
            const maybe_unwrapped = MaybeUnwrap(s);
            if (maybe_unwrapped) | unwrapped | return unwrapped;
            @compileError("can't unwrap a type that is not a resource: " ++ @typeName(T));
        },
        else => @compileError("can't unwrap a type that is not a resource: " ++ @typeName(T)),
    }
}

const ResourceMode = enum { reference, binary };

const ResourceOpts = struct {
    mode: ResourceMode = .reference,
};

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

pub fn resources(comptime root_import: anytype) type {
    return struct {
        pub fn create(data: anytype, opts: ResourceOpts) !Resource(@TypeOf(data)) {
            const T = @TypeOf(data);
            var allocator = Allocator{ .ptr = undefined, .vtable = &resource_vtable };
            var typed_allocator = @ptrCast(**e.ErlNifResourceType, &allocator.ptr);
            root_import.assign_resource_type(T, typed_allocator);

            if (@sizeOf(@TypeOf(data)) == 0) {
                @compileError("you cannot create a resource for a zero-byte object");
            }

            _ = opts;
            const resource_payload = try allocator.create(T);
            resource_payload.* = data;
            return Resource(T){ .resource_payload = resource_payload };
        }

        pub fn unpack(resource: anytype) Unwrap(@TypeOf(resource)) {
            return resource.resource_payload.*;
        }

        pub fn update(resource: anytype, data: Unwrap(@TypeOf(resource))) void {
            resource.resource_payload.* = data;
        }
    };
}

const ResourceInitOpts = struct {
    dtor: ?*e.ErlNifResourceDtor = null,
    stop: ?*e.ErlNifResourceStop = null,
    down: ?*e.ErlNifResourceDown = null,
    dyncall: ?*e.ErlNifResourceDynCall = null,
};

pub fn init(comptime T: type, comptime module: []const u8, env: beam.env, opts: ResourceInitOpts) *e.ErlNifResourceType {
    // load em up.  Note all callbacks are optional and may be set to null.
    // see: https://www.erlang.org/doc/man/erl_nif.html#enif_init_resource_type
    var init_struct: e.ErlNifResourceTypeInit = .{.dtor = null, .stop = null, .down = null, .dyncall = null, .members = 0};
    if (opts.dtor) |dtor| { 
        init_struct.dtor = dtor;
        init_struct.members = 1;
    }

    if (opts.stop) |stop| { 
        init_struct.stop = stop;
        init_struct.members = 2;
    }

    if (opts.down) |down| { 
        init_struct.down = down;
        init_struct.members = 3;
    }

    if (opts.dyncall) |dyncall| { 
        init_struct.dyncall = dyncall;
        init_struct.members = 4;
    }
    
    return if (e.enif_init_resource_type(env, @typeName(T) ++ "-" ++ module, &init_struct, e.ERL_NIF_RT_CREATE, null)) |resource_type| resource_type else @panic("couldn't initialize the resource type for" ++ @typeName(T));
}
