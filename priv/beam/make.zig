const beam = @import("beam.zig");
const e = @import("erl_nif");
const std = @import("std");
const resource = @import("resource.zig");
const options = @import("options.zig");

const OutputType = enum {
    default,
    list,
    binary,
    fn select(opts: anytype) OutputType {
        if (@hasField(@TypeOf(opts), "output_type")) {
            return opts.output;
        } else {
            return .default;
        }
    }
};

pub fn make(value: anytype, opts: anytype) beam.term {
    const T = @TypeOf(value);

    // passthrough on beam.term, no work needed.
    if (T == beam.term) return value;
    if (T == beam.pid) return make_pid(value, opts);
    // special case on stacktrace
    if (T == *std.builtin.StackTrace) return beam.make_stacktrace(value, opts);

    if (T == beam.port) @compileError("you cannot convert a port into a term");

    switch (@typeInfo(T)) {
        .Array => return make_array(value, opts),
        .Pointer => return make_pointer(value, opts),
        .Int => return make_int(value, opts),
        .Struct => return make_struct(value, opts),
        .EnumLiteral => return make_enum_literal(value, opts),
        .Enum => return make_enum(value, opts),
        .ErrorSet => return make_error(value, opts),
        .Null => return make_null(opts),
        .Float => return make_float(value, opts),
        .Bool => return make_bool(value, opts),
        .Optional => return make_optional(value, opts),
        .ComptimeInt => return make_comptime_int(value, opts),
        .ComptimeFloat => return make_comptime_float(value, opts),
        .Void => return make_enum(.ok, opts),
        else => {
            @compileError("unusable type " ++ @typeName(T) ++ " encountered");
        },
    }
}

pub fn make_pid(pid: beam.pid, opts: anytype) beam.term {
    _ = opts;
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
    return .{ .v = @bitCast(pid.pid) };
}

pub fn make_null(opts: anytype) beam.term {
    return .{ .v = e.enif_make_atom(options.env(opts), "nil") };
}

fn make_array(value: anytype, opts: anytype) beam.term {
    return make_array_from_pointer(@TypeOf(value), &value, opts);
}

fn make_pointer(value: anytype, opts: anytype) beam.term {
    const pointer = @typeInfo(@TypeOf(value)).Pointer;
    switch (pointer.size) {
        .One => return make_mut(value, opts),
        .Many => return make_manypointer(value, opts),
        .Slice => return make_slice(value, opts),
        .C => return make_cpointer(value, opts),
    }
}

fn make_optional(value: anytype, opts: anytype) beam.term {
    return if (value) |unwrapped| make(unwrapped, opts) else make_into_atom("nil", opts);
}

fn make_int(value: anytype, opts: anytype) beam.term {
    const int = @typeInfo(@TypeOf(value)).Int;
    switch (int.signedness) {
        .signed => switch (int.bits) {
            0 => return .{ .v = e.enif_make_int(options.env(opts), 0) },
            1...32 => return .{ .v = e.enif_make_int(options.env(opts), @as(i32, @intCast(value))) },
            33...64 => return .{ .v = e.enif_make_int64(options.env(opts), @as(i64, @intCast(value))) },
            else => {},
        },
        .unsigned => switch (int.bits) {
            0 => return .{ .v = e.enif_make_int(options.env(opts), 0) },
            1...32 => return .{ .v = e.enif_make_uint(options.env(opts), @as(u32, @intCast(value))) },
            33...64 => return .{ .v = e.enif_make_uint64(options.env(opts), @as(u64, @intCast(value))) },
            else => {
                const Bigger = std.meta.Int(.unsigned, comptime try std.math.ceilPowerOfTwo(u16, int.bits));
                const buf_size = @sizeOf(Bigger);
                var result: e.ErlNifTerm = undefined;
                var intermediate = @as(Bigger, @intCast(value));
                var buf = e.enif_make_new_binary(options.env(opts), buf_size, &result);

                // transfer content.
                @memcpy(buf[0..buf_size], @as([*]u8, @ptrCast(&intermediate))[0..buf_size]);

                return .{ .v = result };
            },
        },
    }
    unreachable;
}

fn make_comptime_int(value: anytype, opts: anytype) beam.term {
    if (value < std.math.minInt(i64)) {
        @compileError("directly making a value less than min(i64) is not supported");
    }
    if (value < std.math.minInt(i32)) {
        return make_int(@as(i64, value), opts);
    }
    if (value < 0) {
        return make_int(@as(i32, value), opts);
    }
    if (value <= std.math.maxInt(u32)) {
        return make_int(@as(u32, value), opts);
    }
    if (value <= std.math.maxInt(u64)) {
        return make_int(@as(u64, value), opts);
    }
    @compileError("directly making a value greater than max(u64) is not supported");
}

pub fn make_comptime_float(comptime float: comptime_float, opts: anytype) beam.term {
    return beam.make(@as(f64, @floatCast(float)), opts);
}

const EMPTY_TUPLE_LIST = [_]beam.term{};

fn make_struct(value: anytype, opts: anytype) beam.term {
    const struct_info = @typeInfo(@TypeOf(value)).Struct;
    if (struct_info.is_tuple) {
        if (value.len > 16_777_215) {
            @compileError("The tuple size is too large for the erlang virtual machine");
        }
        var tuple_list: [value.len]e.ErlNifTerm = undefined;
        inline for (&tuple_list, 0..) |*tuple_item, index| {
            const tuple_term = value[index];
            if (@TypeOf(tuple_term) == beam.term) {
                tuple_item.* = tuple_term.v;
            } else {
                tuple_item.* = make(tuple_term, opts).v;
            }
        }
        return .{ .v = e.enif_make_tuple_from_array(options.env(opts), &tuple_list, value.len) };
    } else if (resource.MaybeUnwrap(struct_info)) |_| {
        return value.make(opts);
    } else {
        const fields = struct_info.fields;
        var result: e.ErlNifTerm = undefined;
        var keys: [fields.len]e.ErlNifTerm = undefined;
        var vals: [fields.len]e.ErlNifTerm = undefined;

        inline for (fields, 0..) |field, index| {
            if (field.name.len > 255) {
                @compileError("the length of the struct field name is too large for the erlang virtual machine");
            }
            keys[index] = e.enif_make_atom_len(options.env(opts), field.name.ptr, field.name.len);
            vals[index] = make(@field(value, field.name), opts).v;
        }

        _ = e.enif_make_map_from_arrays(options.env(opts), &keys, &vals, fields.len, &result);
        return .{ .v = result };
    }
}

fn make_enum_literal(value: anytype, opts: anytype) beam.term {
    const tag_name = @tagName(value);
    if (tag_name.len > 255) {
        @compileError("the length of this enum literal is too large for the erlang virtual machine");
    }
    return make_into_atom(tag_name, opts);
}

fn make_enum(value: anytype, opts: anytype) beam.term {
    const tag_name = @tagName(value);
    return make_into_atom(tag_name, opts);
}

fn make_error(value: anytype, opts: anytype) beam.term {
    const tag_name = @errorName(value);
    return make_into_atom(tag_name, opts);
}

fn make_float(value: anytype, opts: anytype) beam.term {
    const floatval = @as(f64, @floatCast(value));
    if (std.math.isNan(value)) return make_enum(.NaN, opts);
    if (std.math.isPositiveInf(value)) return make_enum(.infinity, opts);
    if (std.math.isNegativeInf(value)) return make_enum(.neg_infinity, opts);
    return .{ .v = e.enif_make_double(options.env(opts), floatval) };
}

fn make_bool(value: bool, opts: anytype) beam.term {
    return make_into_atom(if (value) "true" else "false", opts);
}

fn make_mut(value_ptr: anytype, opts: anytype) beam.term {
    const child = @TypeOf(value_ptr.*);
    switch (@typeInfo(child)) {
        .Array => return make_array_from_pointer(child, value_ptr, opts),
        .Struct => return make_struct(value_ptr.*, opts),
        else => @compileError("this type is unsupported"),
    }
}

fn make_array_from_pointer(comptime T: type, array_ptr: anytype, opts: anytype) beam.term {
    // u8 arrays (sentinel terminated or otherwise) are treated as
    // strings.
    const array_info = @typeInfo(T).Array;
    const Child = array_info.child;
    const output_type = OutputType.select(opts);

    if (Child == u8 and output_type != .list) {
        // u8 arrays are by default marshalled into binaries.
        return make_binary(array_ptr[0..], opts);
    }

    if (output_type == .binary) {
        // u8 arrays are by default marshalled into binaries.
        return make_binary(array_ptr[0..], opts);
    }

    switch (array_info.child) {
        beam.term => {
            // since beam.term is guaranteed to be a packed struct of only
            // e.ErlNifTerm, this is always guaranteed to work.
            const ptr = @as([*]const e.ErlNifTerm, @ptrCast(array_ptr));
            return .{ .v = e.enif_make_list_from_array(options.env(opts), ptr, array_info.len) };
        },
        e.ErlNifTerm => {
            return .{ .v = e.enif_make_list_from_array(options.env(opts), array_ptr, array_info.len) };
        },
        // the general case is build the list backwards.
        else => {
            var tail = e.enif_make_list_from_array(options.env(opts), null, 0);

            if (array_info.len != 0) {
                var index: usize = array_info.len;
                while (index > 0) : (index -= 1) {
                    tail = e.enif_make_list_cell(options.env(opts), make(array_ptr[index - 1], opts).v, tail);
                }
            }

            return .{ .v = tail };
        },
    }
}

pub fn make_manypointer(manypointer: anytype, opts: anytype) beam.term {
    const pointer = @typeInfo(@TypeOf(manypointer)).Pointer;
    if (pointer.sentinel) |_| {
        const len = std.mem.len(manypointer);
        return make_slice(manypointer[0..len], opts);
    } else {
        @compileError("it's not possible to create a manypointer without a sentinel");
    }
}

fn make_cpointer(cpointer: anytype, opts: anytype) beam.term {
    const pointer = @typeInfo(@TypeOf(cpointer)).Pointer;
    const Child = pointer.child;

    if (cpointer) |_| {
        // the following two types have inferrable sentinels
        if (Child == u8) {
            return make(@as([*:0]u8, @ptrCast(cpointer)), opts);
        }
        if (@typeInfo(Child) == .Pointer) {
            return make(@as([*:null]Child, @ptrCast(cpointer)), opts);
        }

        switch (@typeInfo(Child)) {
            .Struct => return make(@as(*Child, @ptrCast(cpointer)), opts),
            else => @compileError("this is not supported"),
        }
    } else {
        return make(.nil, opts);
    }
}

pub fn make_slice(slice: anytype, opts: anytype) beam.term {
    const output_type = OutputType.select(opts);
    const SliceType = @TypeOf(slice);
    // u8 slices default to binary and must be opt-in to get charlists out.
    if ((SliceType == []u8 or SliceType == []const u8) and output_type != .list) {
        return make_binary(slice, opts);
    }

    // any other slices can be opt-in to get a binary out
    // TODO: check to make sure that these are either ints, floats, or packed structs.
    if (output_type == .binary) {
        return make_binary(slice, opts);
    }

    switch (@TypeOf(slice)) {
        []beam.term => {
            // since beam.term is guaranteed to be a packed struct of only
            // e.ErlNifTerm, this is always guaranteed to work.
            const ptr = @as([*]const e.ErlNifTerm, @ptrCast(slice.ptr));
            return .{ .v = e.enif_make_list_from_array(options.env(opts), ptr, @as(c_uint, @intCast(slice.len))) };
        },
        []e.ErlNifTerm => {
            return .{ .v = e.enif_make_list_from_array(options.env(opts), slice.ptr, @as(c_uint, @intCast(slice.len))) };
        },
        else => {
            var tail = e.enif_make_list_from_array(options.env(opts), null, 0);

            if (slice.len != 0) {
                var index = slice.len;
                while (index > 0) : (index -= 1) {
                    tail = e.enif_make_list_cell(options.env(opts), make(slice[index - 1], opts).v, tail);
                }
            }

            return .{ .v = tail };
        },
    }
}

pub fn make_into_atom(atom_string: []const u8, opts: anytype) beam.term {
    return .{ .v = e.enif_make_atom_len(options.env(opts), atom_string.ptr, atom_string.len) };
}

fn make_binary(content: anytype, opts: anytype) beam.term {
    const T = @TypeOf(content);
    switch (@typeInfo(T)) {
        .Pointer => |P| {
            const Child = P.child;
            switch (P.size) {
                .Slice => {
                    const byte_size = @sizeOf(Child) * content.len;
                    const u8buf = @as([*]const u8, @ptrCast(content.ptr));
                    return make_binary_from_u8_slice(u8buf[0..byte_size], opts);
                },
                // it is possible that this is a const pointer to an array in memory.
                .One => {
                    if (@typeInfo(Child) != .Array) {
                        @compileError("make_binary is only supported for array and slice pointers");
                    }
                    const byte_size = @sizeOf(@typeInfo(Child).Array.child) * content.len;
                    const u8buf = @as([*]const u8, @ptrCast(content));
                    return make_binary_from_u8_slice(u8buf[0..byte_size], opts);
                },
                else => @compileError("make_binary is only supported for array and slice pointers"),
            }
        },
        .Array => |A| {
            const byte_size = @sizeOf(A.child) * content.len;
            const u8buf = @as([*]const u8, @ptrCast(&content));
            return make_binary_from_u8_slice(u8buf[0..byte_size], opts);
        },
        else => @compileError("make_binary is only supported for slices and arrays"),
    }
}

fn make_binary_from_u8_slice(slice: []const u8, opts: anytype) beam.term {
    var result: beam.term = undefined;
    var buf = e.enif_make_new_binary(options.env(opts), slice.len, &result.v);
    @memcpy(buf[0..slice.len], slice);
    return result;
}

pub fn make_empty_list(opts: anytype) beam.term {
    return .{ .v = e.enif_make_list_from_array(options.env(opts), null, 0) };
}

pub fn make_list_cell(head: beam.term, tail: beam.term, opts: anytype) beam.term {
    return .{ .v = e.enif_make_list_cell(options.env(opts), head.v, tail.v) };
}

pub fn make_error_pair(payload: anytype, opts: anytype) beam.term {
    return make(.{ .@"error", make(payload, opts) }, opts);
}

pub fn make_error_atom(opts: anytype) beam.term {
    return make(.@"error", opts);
}

pub fn make_ref(opts: anytype) beam.term {
    return .{ .v = e.enif_make_ref(options.env(opts)) };
}
