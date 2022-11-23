const beam = @import("beam.zig");
const e = @import("erl_nif.zig");
const std = @import("std");

const OutputType = enum { default, charlists, noclean };
const MakeOpts = struct {
    output_as: OutputType = .default,
};

pub fn make(env: beam.env, value: anytype, comptime opts: MakeOpts) beam.term {
    const T = @TypeOf(value);
    // passthrough on beam.term and e.ErlNifTerm, no work needed.
    if (T == beam.term) return value;
    if (T == e.ErlNifTerm) return .{ .v = value };

    switch (@typeInfo(T)) {
        .Array => return make_array(env, value, opts),
        .Pointer => return make_pointer(env, value, opts),
        .Int => return make_int(env, value),
        .Struct => return make_struct(env, value, opts),
        .EnumLiteral => return make_enum_literal(env, value),
        .Enum => return make_enum(env, value),
        .ErrorSet => return make_error(env, value),
        .Null => return make_null(env),
        .Float => return make_float(env, value),
        .Bool => return make_bool(env, value),
        .Optional => return make_optional(env, value, opts),
        .ComptimeInt => return make_comptime_int(env, value),
        .ComptimeFloat => return make_comptime_float(env, value),
        else => {
            @compileError("unusable type encountered");
        },
    }
}

pub fn make_null(env: beam.env) beam.term {
    return .{ .v = e.enif_make_atom(env, "nil") };
}

fn make_array(env: beam.env, value: anytype, comptime opts: MakeOpts) beam.term {
    return make_array_from_pointer(@TypeOf(value), env, &value, opts);
}

fn make_pointer(env: beam.env, value: anytype, comptime opts: MakeOpts) beam.term {
    const pointer = @typeInfo(@TypeOf(value)).Pointer;
    switch (pointer.size) {
        .One => return make_mut(env, value, opts),
        .Many => return make_manypointer(env, value, opts),
        .Slice => return make_slice(env, value, opts),
        .C => return make_cpointer(env, value, opts),
    }
}

fn make_optional(env: beam.env, value: anytype, comptime opts: MakeOpts) beam.term {
    return if (value) |unwrapped| make(env, unwrapped, opts) else make_into_atom(env, "nil");
}

fn make_int(env: beam.env, value: anytype) beam.term {
    const int = @typeInfo(@TypeOf(value)).Int;
    switch (int.signedness) {
        .signed => switch (int.bits) {
            0 => return .{ .v = e.enif_make_int(env, 0)},
            1...32 => return .{ .v = e.enif_make_int(env, @intCast(i32, value)) },
            33...64 => return .{ .v = e.enif_make_int64(env, @intCast(i64, value)) },
            else => {},
        },
        .unsigned => switch (int.bits) {
            0 => return .{ .v = e.enif_make_int(env, 0)},
            1...32 => return .{ .v = e.enif_make_uint(env, @intCast(u32, value)) },
            33...64 => return .{ .v = e.enif_make_uint64(env, @intCast(u64, value)) },
            else => {
                const Bigger = std.meta.Int(.unsigned, comptime try std.math.ceilPowerOfTwo(u16, int.bits));
                const buf_size = @sizeOf(Bigger);
                var result: e.ErlNifTerm = undefined;
                var intermediate = @intCast(Bigger, value);
                var buf = e.enif_make_new_binary(env, buf_size, &result);

                // transfer content.
                std.mem.copy(u8, buf[0..buf_size], @ptrCast([*]u8, &intermediate)[0..buf_size]);

                return .{ .v = result };
            },
        },
    }
    unreachable;
}

fn make_comptime_int(env: beam.env, value: anytype) beam.term {
    if (value < std.math.minInt(i64)) {
        @compileError("directly making a value less than i64 is not supported");
    }
    if (value < std.math.minInt(i32)) {
        return make_int(env, @as(i64, value));
    }
    if (value < 0) {
        return make_int(env, @as(i32, value));
    }
    if (value <= std.math.maxInt(u32)) {
        return make_int(env, @as(u32, value));
    }
    if (value <= std.math.maxInt(u64)) {
        return make_int(env, @as(u64, value));
    }
    @compileError("directly making a value greater than u64 is not supported");
}

pub fn make_comptime_float(env: beam.env, comptime float: comptime_float) beam.term {
    return beam.make(env, @floatCast(f64, float));
}

const EMPTY_TUPLE_LIST = [_]beam.term{};

fn make_struct(env: beam.env, value: anytype, comptime opts: MakeOpts) beam.term {
    const struct_info = @typeInfo(@TypeOf(value)).Struct;
    if (struct_info.is_tuple) {
        if (value.len > 16_777_215) {
            @compileError("The tuple size is too large for the erlang virtual machine");
        }
        var tuple_list: [value.len]e.ErlNifTerm = undefined;
        comptime var index = 0;
        inline while (index < value.len) : (index += 1) {
            const tuple_term = value[index];
            if (@TypeOf(tuple_term) == beam.term) {
                tuple_list[index] = tuple_term.v;
            } else {
                tuple_list[index] = make(env, tuple_term, opts).v;
            }
        }
        return .{ .v = e.enif_make_tuple_from_array(env, &tuple_list, value.len) };
    } else {
        const fields = struct_info.fields;
        var result: e.ErlNifTerm = undefined;
        var keys: [fields.len]e.ErlNifTerm = undefined;
        var vals: [fields.len]e.ErlNifTerm = undefined;

        inline for (fields) |field, index| {
            if (field.name.len > 255) {
                @compileError("the length of the struct field name is too large for the erlang virtual machine");
            }
            keys[index] = e.enif_make_atom_len(env, field.name.ptr, field.name.len);
            vals[index] = make(env, @field(value, field.name), opts).v;
        }

        _ = e.enif_make_map_from_arrays(env, &keys, &vals, fields.len, &result);
        return .{ .v = result };
    }
}

fn make_enum_literal(env: beam.env, value: anytype) beam.term {
    const tag_name = @tagName(value);
    if (tag_name.len > 255) {
        @compileError("the length of this enum literal is too large for the erlang virtual machine");
    }
    return make_into_atom(env, tag_name);
}

fn make_enum(env: beam.env, value: anytype) beam.term {
    const tag_name = @tagName(value);
    return make_into_atom(env, tag_name);
}

fn make_error(env: beam.env, value: anytype) beam.term {
    const tag_name = @errorName(value);
    return make_into_atom(env, tag_name);
}

fn make_float(env: beam.env, value: anytype) beam.term {
    const floatval = @floatCast(f64, value);
    if (std.math.isNan(value)) return make_enum(env, .NaN);
    if (std.math.isPositiveInf(value)) return make_enum(env, .infinity);
    if (std.math.isNegativeInf(value)) return make_enum(env, .neg_infinity);
    return .{ .v = e.enif_make_double(env, floatval) };
}

fn make_bool(env: beam.env, value: bool) beam.term {
    return make_into_atom(env, if (value) "true" else "false");
}

fn make_mut(env: beam.env, value_ptr: anytype, comptime opts: MakeOpts) beam.term {
    const child = @TypeOf(value_ptr.*);
    switch (@typeInfo(child)) {
        .Array => return make_array_from_pointer(child, env, value_ptr, opts),
        .Struct => return make_struct(env, value_ptr.*, opts),
        else => @compileError("this type is unsupported"),
    }
}

fn make_array_from_pointer(comptime T: type, env: beam.env, array_ptr: anytype, comptime opts: MakeOpts) beam.term {
    // u8 arrays (sentinel terminated or otherwise) are treated as
    // strings.
    const array_info = @typeInfo(T).Array;
    const Child = array_info.child;

    if (Child == u8 and opts.output_as != .charlists) { 
        // u8 arrays are by default marshalled into binaries.
        return make_binary(env, array_ptr[0..]);
    }

    switch (array_info.child) {
        beam.term => {
            // since beam.term is guaranteed to be a packed struct of only
            // e.ErlNifTerm, this is always guaranteed to work.
            const ptr = @ptrCast([*]const e.ErlNifTerm, array_ptr);
            return .{ .v = e.enif_make_list_from_array(env, ptr, array_info.len) };
        },
        e.ErlNifTerm => {
            return .{ .v = e.enif_make_list_from_array(env, array_ptr, array_info.len) };
        },
        // the general case is build the list backwards.
        else => {
            var tail = e.enif_make_list_from_array(env, null, 0);

            if (array_info.len != 0) {
                var index: usize = array_info.len;
                while (index > 0) : (index -= 1) {
                    tail = e.enif_make_list_cell(env, make(env, array_ptr[index - 1], opts).v, tail);
                }
            }

            return .{ .v = tail };
        },
    }
}

pub fn make_manypointer(env: beam.env, manypointer: anytype, comptime opts: MakeOpts) beam.term {
    const pointer = @typeInfo(@TypeOf(manypointer)).Pointer;
    if (pointer.sentinel) |_| {    
        const len = std.mem.len(manypointer);
        return make_slice(env, manypointer[0..len], opts);
    } else {
        @compileError("it's not possible to create a manypointer without a sentinel");
    }
}

pub fn make_cpointer(env: beam.env, cpointer: anytype, comptime opts: MakeOpts) beam.term {
    const pointer = @typeInfo(@TypeOf(cpointer)).Pointer;
    const Child = pointer.child;

    if (cpointer) |_| {
        // the following two types have inferrable sentinels
        if (Child == u8) {
            return make(env, @ptrCast([*:0]u8, cpointer), opts);
        }
        if (@typeInfo(Child) == .Pointer) {
            return make(env, @ptrCast([*:null]Child, cpointer), opts);
        }

        switch (@typeInfo(Child)) {
            .Struct =>
              return make(env, @ptrCast(*Child, cpointer), opts),
            else => 
              @compileError("this is not supported")
        }
    } else {
        return make(env, .nil, opts);
    }
}

pub fn make_slice(env: beam.env, slice: anytype, comptime opts: MakeOpts) beam.term {
    if (@TypeOf(slice) == []u8 and opts.output_as != .charlists) {
        return make_binary(env, slice);
    }

    switch (@TypeOf(slice)) {
        []beam.term => {
            // since beam.term is guaranteed to be a packed struct of only
            // e.ErlNifTerm, this is always guaranteed to work.
            const ptr = @ptrCast([*]const e.ErlNifTerm, slice.ptr);
            return .{ .v = e.enif_make_list_from_array(env, ptr, @intCast(c_uint, slice.len)) };
        },
        []e.ErlNifTerm => {
            return .{ .v = e.enif_make_list_from_array(env, slice.ptr, @intCast(c_uint, slice.len)) };
        },
        else => {
            var tail = e.enif_make_list_from_array(env, null, 0);

            if (slice.len != 0) {
                var index = slice.len;
                while (index > 0) : (index -= 1) {
                    tail = e.enif_make_list_cell(env, make(env, slice[index - 1], opts).v, tail);
                }
            }

            return .{ .v = tail };
        },
    }
}

pub fn make_into_atom(env: beam.env, atom_string: []const u8) beam.term {
    return .{ .v = e.enif_make_atom_len(env, atom_string.ptr, atom_string.len) };
}

pub fn make_binary(env: beam.env, binary_slice: []const u8) beam.term {
    // TODO: make this return some sort of error if it fails
    var result: beam.term = undefined;
    var buf = e.enif_make_new_binary(env, binary_slice.len, &result.v);
    std.mem.copy(u8, buf[0..binary_slice.len], binary_slice);
    return result;
}
