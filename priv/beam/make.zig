const beam = @import("beam.zig");
const e = @import("erl_nif.zig");
const std = @import("std");
const resource = beam.resource;

const OutputType = enum {
    default,
    charlists,
    binary,
    fn select(opts: anytype) OutputType {
        if (@hasField(@TypeOf(opts), "output_type")) {
            return opts.output_type;
        } else {
            return .default;
        }
    }
};

pub fn make(env: beam.env, value: anytype, comptime opts: anytype) beam.term {
    const T = @TypeOf(value);

    // passthrough on beam.term, no work needed.
    if (T == beam.term) return value;
    if (T == beam.pid) return make_pid(env, value);
    if (T == beam.port) @compileError("you cannot convert a port into a term");

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
        .Void => return make_enum(env, .ok),
        else => {
            @compileError("unusable type encountered");
        },
    }
}

pub fn make_pid(_: beam.env, pid: beam.pid) beam.term {
    // for some reason, the macro for this is super shitty.  See:
    // https://github.com/erlang/otp/blob/a78b8e0769bba69ba1245cce850b226bdf6940fa/erts/emulator/beam/erl_nif_api_funcs.h#L649
    // so we have to use secret hidden implementation, which is
    // that the pid struct just wraps beam.term directly =(

    // note that we also have just disable this in the sema step,
    // because sema shims pid with a bogus extern struct, which
    // can't have pid.
    beam.ignore_when_sema();

    // super bogus and super sketchy code:
    // this code will fail to work if the OTP team ever changes
    // their internal pid representation away from this!
    return .{ .v = @bitCast(e.ErlNifTerm, pid.pid) };
}

pub fn make_null(env: beam.env) beam.term {
    return .{ .v = e.enif_make_atom(env, "nil") };
}

fn make_array(env: beam.env, value: anytype, comptime opts: anytype) beam.term {
    return make_array_from_pointer(@TypeOf(value), env, &value, opts);
}

fn make_pointer(env: beam.env, value: anytype, comptime opts: anytype) beam.term {
    const pointer = @typeInfo(@TypeOf(value)).Pointer;
    switch (pointer.size) {
        .One => return make_mut(env, value, opts),
        .Many => return make_manypointer(env, value, opts),
        .Slice => return make_slice(env, value, opts),
        .C => return make_cpointer(env, value, opts),
    }
}

fn make_optional(env: beam.env, value: anytype, comptime opts: anytype) beam.term {
    return if (value) |unwrapped| make(env, unwrapped, opts) else make_into_atom(env, "nil");
}

fn make_int(env: beam.env, value: anytype) beam.term {
    const int = @typeInfo(@TypeOf(value)).Int;
    switch (int.signedness) {
        .signed => switch (int.bits) {
            0 => return .{ .v = e.enif_make_int(env, 0) },
            1...32 => return .{ .v = e.enif_make_int(env, @intCast(i32, value)) },
            33...64 => return .{ .v = e.enif_make_int64(env, @intCast(i64, value)) },
            else => {},
        },
        .unsigned => switch (int.bits) {
            0 => return .{ .v = e.enif_make_int(env, 0) },
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
    return beam.make(env, @floatCast(f64, float), .{});
}

const EMPTY_TUPLE_LIST = [_]beam.term{};

fn make_struct(env: beam.env, value: anytype, comptime opts: anytype) beam.term {
    const struct_info = @typeInfo(@TypeOf(value)).Struct;
    if (struct_info.is_tuple) {
        if (value.len > 16_777_215) {
            @compileError("The tuple size is too large for the erlang virtual machine");
        }
        var tuple_list: [value.len]e.ErlNifTerm = undefined;
        inline for (tuple_list) |*tuple_item, index| {
            const tuple_term = value[index];
            if (@TypeOf(tuple_term) == beam.term) {
                tuple_item.* = tuple_term.v;
            } else {
                tuple_item.* = make(env, tuple_term, opts).v;
            }
        }
        return .{ .v = e.enif_make_tuple_from_array(env, &tuple_list, value.len) };
    } else if (resource.MaybeUnwrap(struct_info)) |_| {
        return value.make(env, opts);
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

fn make_mut(env: beam.env, value_ptr: anytype, comptime opts: anytype) beam.term {
    const child = @TypeOf(value_ptr.*);
    switch (@typeInfo(child)) {
        .Array => return make_array_from_pointer(child, env, value_ptr, opts),
        .Struct => return make_struct(env, value_ptr.*, opts),
        else => @compileError("this type is unsupported"),
    }
}

fn make_array_from_pointer(comptime T: type, env: beam.env, array_ptr: anytype, comptime opts: anytype) beam.term {
    // u8 arrays (sentinel terminated or otherwise) are treated as
    // strings.
    const array_info = @typeInfo(T).Array;
    const Child = array_info.child;
    const output_as = OutputType.select(opts);

    if (Child == u8 and output_as != .charlists) {
        // u8 arrays are by default marshalled into binaries.
        return make_binary(env, array_ptr[0..]);
    }

    if (output_as == .binary) {
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

pub fn make_manypointer(env: beam.env, manypointer: anytype, comptime opts: anytype) beam.term {
    const pointer = @typeInfo(@TypeOf(manypointer)).Pointer;
    if (pointer.sentinel) |_| {
        const len = std.mem.len(manypointer);
        return make_slice(env, manypointer[0..len], opts);
    } else {
        @compileError("it's not possible to create a manypointer without a sentinel");
    }
}

pub fn make_cpointer(env: beam.env, cpointer: anytype, comptime opts: anytype) beam.term {
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
            .Struct => return make(env, @ptrCast(*Child, cpointer), opts),
            else => @compileError("this is not supported"),
        }
    } else {
        return make(env, .nil, opts);
    }
}

pub fn make_slice(env: beam.env, slice: anytype, comptime opts: anytype) beam.term {
    const output_as = OutputType.select(opts);
    // u8 slices default to binary and must be opt-in to get charlists out.
    if (@TypeOf(slice) == []u8 and output_as != .charlists) {
        return make_binary(env, slice);
    }

    // any other slices can be opt-in to get a binary out
    // TODO: check to make sure that these are either ints, floats, or packed structs.
    if (output_as == .binary) {
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

pub fn make_binary(env: beam.env, content: anytype) beam.term {
    const T = @TypeOf(content);
    switch (@typeInfo(T)) {
        .Pointer => |P| {
            const Child = P.child;
            switch (P.size) {
                .Slice => {
                    const byte_size = @sizeOf(Child) * content.len;
                    const u8buf = @ptrCast([*]const u8, content.ptr);
                    return make_binary_from_u8_slice(env, u8buf[0..byte_size]);
                },
                // it is possible that this is a const pointer to an array in memory.
                .One => {
                    if (@typeInfo(Child) != .Array) {
                        @compileError("make_binary is only supported for array and slice pointers");
                    }
                    const byte_size = @sizeOf(@typeInfo(Child).Array.child) * content.len;
                    const u8buf = @ptrCast([*]const u8, content);
                    return make_binary_from_u8_slice(env, u8buf[0..byte_size]);
                },
                else => @compileError("make_binary is only supported for array and slice pointers"),
            }
        },
        .Array => |A| {
            const byte_size = @sizeOf(A.child) * content.len;
            const u8buf = @ptrCast([*]const u8, &content);
            return make_binary_from_u8_slice(env, u8buf[0..byte_size]);
        },
        else => @compileError("make_binary is only supported for slices and arrays"),
    }
}

fn make_binary_from_u8_slice(env: beam.env, slice: []const u8) beam.term {
    var result: beam.term = undefined;
    var buf = e.enif_make_new_binary(env, slice.len, &result.v);
    std.mem.copy(u8, buf[0..slice.len], slice);
    return result;
}

pub fn make_empty_list(env: beam.env) beam.term {
    return .{ .v = e.enif_make_list_from_array(env, null, 0) };
}

// you can't make the atom .error, because `error` is a reserved term
// in ziglang.
pub fn make_error_atom(env: beam.env) beam.term {
    return make_into_atom(env, "error");
}

pub fn make_error_pair(env: beam.env, payload: anytype, comptime opts: anytype) beam.term {
    return make(env, .{ make_error_atom(env), make(env, payload, opts) }, opts);
}
