//! Compatibility shim for Zig 0.17 struct-of-arrays type reflection.
//!
//! In Zig 0.17, `@typeInfo` reports struct/union/enum members as parallel arrays
//! (`field_names`, `field_types`, `field_attrs` / `field_values`) rather than as a
//! single array of per-field records.  The BEAM marshalling code reflects over user
//! types in a great many places and reads several field properties at once, so this
//! module rebuilds the per-field view at comptime.
//!
//! Everything here is comptime-only: the returned slices are comptime-known, so
//! `field.name` remains usable in `@field(...)` and `field.type` remains usable as a
//! type, exactly as before.

const std = @import("std");

/// A single struct or union field, in the pre-0.17 "array of records" shape.
pub const Field = struct {
    name: [:0]const u8,
    type: type,
    /// Type-erased pointer to the field's default value, or null when it has none.
    /// Prefer `defaultValue` to read it.
    default_value_ptr: ?*const anyopaque = null,
    /// The field's effective alignment: the explicit alignment when one was given,
    /// otherwise the natural alignment of the field type.
    alignment: comptime_int = 0,
    is_comptime: bool = false,

    /// Loads this field's default value, or null when it has none.
    pub inline fn defaultValue(comptime field: Field) ?field.type {
        const ptr: *const field.type = @ptrCast(@alignCast(field.default_value_ptr orelse return null));
        return ptr.*;
    }
};

/// A single enum member, in the pre-0.17 "array of records" shape.
pub const EnumField = struct {
    name: [:0]const u8,
    value: comptime_int,
};

/// The fields of a struct, as an array of records.
pub inline fn fields(comptime T: type) []const Field {
    return structFields(@typeInfo(T).@"struct");
}

/// The fields of a union, as an array of records.
pub inline fn unionFields(comptime T: type) []const Field {
    return containerFields(@typeInfo(T).@"union");
}

/// The fields of an already-destructured `std.lang.Type.Struct`.
pub inline fn structFields(comptime info: std.lang.Type.Struct) []const Field {
    return containerFields(info);
}

/// The members of an enum, as an array of records.
pub inline fn enumFields(comptime T: type) []const EnumField {
    return enumInfoFields(@typeInfo(T).@"enum");
}

/// The members of an already-destructured `std.lang.Type.Enum`.
pub inline fn enumInfoFields(comptime info: std.lang.Type.Enum) []const EnumField {
    comptime {
        var out: [info.field_names.len]EnumField = undefined;
        for (info.field_names, info.field_values, 0..) |name, value, index| {
            out[index] = .{ .name = name, .value = value };
        }
        const frozen = out;
        return &frozen;
    }
}

/// Shared implementation: structs and unions both carry the parallel-array shape, but
/// union fields have no default or `comptime` attribute, so those are probed for.
inline fn containerFields(comptime info: anytype) []const Field {
    comptime {
        // Derive the attribute type from the slice rather than an element:
        // an empty struct has no field_attrs[0] to index.
        const Attrs = @typeInfo(@TypeOf(info.field_attrs)).pointer.child;
        var out: [info.field_names.len]Field = undefined;
        for (info.field_names, info.field_types, info.field_attrs, 0..) |name, @"type", attrs, index| {
            out[index] = .{
                .name = name,
                .type = @"type",
                .default_value_ptr = if (@hasField(Attrs, "default_value_ptr")) attrs.default_value_ptr else null,
                .alignment = attrs.@"align" orelse @alignOf(@"type"),
                .is_comptime = @hasField(Attrs, "comptime") and attrs.@"comptime",
            };
        }
        const frozen = out;
        return &frozen;
    }
}
