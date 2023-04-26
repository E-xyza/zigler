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

fn ResourceAllocator(comptime root_import: anytype, comptime T: type) type {
    return struct {
        allocator: Allocator = .{
            .ptr = root_import.resource_lookup(T),
            .vtable = &resource_vtable,
        },
    };
}

pub fn resources(comptime root_import: anytype) type {
    return struct {
        pub fn create(data: anytype, opts: ResourceOpts) !Resource(@TypeOf(data)) {
            const resource_allocator: ResourceAllocator(root_import, @TypeOf(data)) = .{};
            const allocator = resource_allocator.allocator;

            if (@sizeOf(@TypeOf(data)) == 0) {
                @compileError("you cannot create a resource for a zero-byte object");
            }

            _ = opts;
            const T = @TypeOf(data);
            const resource_payload = try allocator.create(T);
            resource_payload.* = data;
            return Resource(T){ .resource_payload = resource_payload };
        }
    };
}
