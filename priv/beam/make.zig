const beam = @import("beam.zig");
const e = @import("erl_nif");
const std = @import("std");
const resource = @import("resource.zig");
const options = @import("options.zig");

pub fn make(value: anytype, opts: anytype) beam.term {
    const T = @TypeOf(value);

    // passthrough on beam.term, no work needed.
    if (T == beam.term) {
        options.assert_default(T, opts);
        return value;
    }

    if (T == beam.pid) {
        options.assert_default(T, opts);
        return make_pid(value, opts);
    }

    // special case on stacktrace
    if (T == *std.builtin.StackTrace) {
        options.assert_default(T, opts);
        return beam.make_stacktrace(value, opts);
    }

    if (T == beam.port) @compileError("you cannot convert a port into a term");

    switch (@typeInfo(T)) {
        .array => return make_array(value, opts),
        .pointer => return make_pointer(value, opts),
        .int => return make_int(value, opts),
        .@"struct" => return make_struct(value, opts),
        .enum_literal => return make_enum_literal(value, opts),
        .@"enum" => return make_enum(value, opts),
        .error_set => return make_error(value, opts),
        .null => return make_null(opts),
        .float => return make_float(value, opts),
        .bool => return make_bool(value, opts),
        .optional => return make_optional(value, opts),
        .comptime_int => return make_comptime_int(value, opts),
        .comptime_float => return make_comptime_float(value, opts),
        .void => return make_enum(.ok, opts),
        else => {
            @compileError("unusable type " ++ @typeName(T) ++ " encountered");
        },
    }
}

pub fn make_pid(pid: beam.pid, opts: anytype) beam.term {
    options.assert_default(beam.pid, opts);

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
    const pointer = @typeInfo(@TypeOf(value)).pointer;
    switch (pointer.size) {
        .one => return make_mut(value, opts),
        .many => return make_manypointer(value, opts),
        .slice => return make_slice(value, opts),
        .c => return make_cpointer(value, opts),
    }
}

fn make_optional(value: anytype, opts: anytype) beam.term {
    return if (value) |unwrapped| make(unwrapped, opts) else make_into_atom("nil", opts);
}

fn make_int(value: anytype, opts: anytype) beam.term {
    options.assert_default(@TypeOf(value), opts);

    const int = @typeInfo(@TypeOf(value)).int;
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
    options.assert_default(@TypeOf(value), opts);

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
    options.assert_default(comptime_float, opts);
    return beam.make(@as(f64, @floatCast(float)), opts);
}

const EMPTY_TUPLE_LIST = [_]beam.term{};

fn make_struct(value: anytype, opts: anytype) beam.term {
    const struct_info = @typeInfo(@TypeOf(value)).@"struct";
    const env = options.env(opts);
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
        return .{ .v = e.enif_make_tuple_from_array(env, &tuple_list, value.len) };
    } else if (resource.MaybeUnwrap(struct_info)) |_| {
        return value.make(opts);
    } else {
        return switch (options.output(opts)) {
            .binary => make_struct_binary(value, opts),
            .default => if (struct_info.layout == .@"packed") make_struct_binary(value, opts) else make_struct_map(value, opts),
            else => make_struct_map(value, opts),
        };
    }
}

fn make_struct_map(value: anytype, opts: anytype) beam.term {
    const struct_info = @typeInfo(@TypeOf(value)).@"struct";
    const env = options.env(opts);
    const fields = struct_info.fields;
    var result: e.ErlNifTerm = undefined;
    var keys: [fields.len]e.ErlNifTerm = undefined;
    var vals: [fields.len]e.ErlNifTerm = undefined;

    inline for (fields, 0..) |field, index| {
        // this needs to be implemented.
        const map_child_as = if (@hasField(@TypeOf(opts), "as")) options.map_child(opts.as, field.name) else .default;
        const child_opts = .{ .env = env, .as = map_child_as };

        if (field.name.len > 255) {
            @compileError("the length of the struct field name is too large for the erlang virtual machine");
        }
        keys[index] = e.enif_make_atom_len(env, field.name.ptr, field.name.len);
        vals[index] = make(@field(value, field.name), child_opts).v;
    }

    _ = e.enif_make_map_from_arrays(env, &keys, &vals, fields.len, &result);

    if (@hasField(@TypeOf(opts), "struct")) {
        const dunder_struct = make_into_atom("__struct__", opts);
        const module_atom = make(opts.@"struct", opts);
        _ = e.enif_make_map_put(env, result, dunder_struct.v, module_atom.v, &result);
    }

    return .{ .v = result };
}

fn make_struct_binary(value: anytype, opts: anytype) beam.term {
    const T = @TypeOf(value);
    const struct_info = @typeInfo(T).@"struct";
    switch (struct_info.layout) {
        .@"packed", .@"extern" => {},
        else => @compileError("only packed and extern structs can be output as binary"),
    }
    const binary_size = @sizeOf(T);
    var result: beam.term = undefined;
    const buf: *T = @ptrCast(@alignCast(e.enif_make_new_binary(options.env(opts), binary_size, &result.v)));
    buf.* = value;
    return result;
}

fn make_enum_literal(value: anytype, opts: anytype) beam.term {
    options.assert_default(@TypeOf(value), opts);
    const tag_name = @tagName(value);
    if (tag_name.len > 255) {
        @compileError("the name of this enum literal is too large for the erlang virtual machine");
    }
    return make_into_atom(tag_name, opts);
}

fn make_enum(value: anytype, opts: anytype) beam.term {
    const new_opts = .{ .env = options.env(opts), .as = .default };
    switch (options.output(opts)) {
        .default => return make_into_atom(@tagName(value), new_opts),
        .integer => return make_int(@intFromEnum(value), new_opts),
        else => |output| {
            const msg = std.fmt.comptimePrint("the 'as' field for an enum must be `.default` or `.integer`, got {}.", .{output});
            @compileError(msg);
        },
    }
}

fn make_error(value: anytype, opts: anytype) beam.term {
    options.assert_default(@TypeOf(value), opts);
    const tag_name = @errorName(value);
    return make_into_atom(tag_name, opts);
}

fn make_float(value: anytype, opts: anytype) beam.term {
    options.assert_default(@TypeOf(value), opts);
    const floatval = @as(f64, @floatCast(value));
    if (std.math.isNan(value)) return make_enum(.NaN, opts);
    if (std.math.isPositiveInf(value)) return make_enum(.infinity, opts);
    if (std.math.isNegativeInf(value)) return make_enum(.neg_infinity, opts);
    return .{ .v = e.enif_make_double(options.env(opts), floatval) };
}

fn make_bool(value: bool, opts: anytype) beam.term {
    options.assert_default(bool, opts);
    return make_into_atom(if (value) "true" else "false", opts);
}

fn make_mut(value_ptr: anytype, opts: anytype) beam.term {
    const child = @TypeOf(value_ptr.*);
    switch (@typeInfo(child)) {
        .array => return make_array_from_pointer(child, value_ptr, opts),
        .@"struct" => return make_struct(value_ptr.*, opts),
        else => @compileError("this type is unsupported"),
    }
}

fn make_array_from_pointer(comptime T: type, array_ptr: anytype, opts: anytype) beam.term {
    const array_info = @typeInfo(T).array;
    const Child = array_info.child;

    // u8 arrays (sentinel terminated or otherwise) are treated as
    // strings.
    if (Child == u8 and options.output(opts) != .list) {
        return make_binary(array_ptr[0..], opts);
    }

    if (options.output(opts) == .binary) {
        // u8 arrays are by default marshalled into binaries.
        return make_binary(array_ptr[0..], opts);
    }

    const env = options.env(opts);
    switch (array_info.child) {
        beam.term => {
            // since beam.term is guaranteed to be a packed struct of only
            // e.ErlNifTerm, this is always guaranteed to work.
            const ptr = @as([*]const e.ErlNifTerm, @ptrCast(array_ptr));
            return .{ .v = e.enif_make_list_from_array(env, ptr, array_info.len) };
        },
        e.ErlNifTerm => {
            return .{ .v = e.enif_make_list_from_array(env, array_ptr, array_info.len) };
        },
        // the general case is build the list backwards.
        else => {
            const list_child_as = if (@hasField(@TypeOf(opts), "as")) options.list_child(opts.as) else .default;
            const child_opts = .{ .env = env, .as = list_child_as };
            var tail = e.enif_make_list_from_array(env, null, 0);

            if (array_info.len != 0) {
                var index: usize = array_info.len;
                while (index > 0) : (index -= 1) {
                    tail = e.enif_make_list_cell(env, make(array_ptr[index - 1], child_opts).v, tail);
                }
            }

            return .{ .v = tail };
        },
    }
}

pub fn make_manypointer(manypointer: anytype, opts: anytype) beam.term {
    const pointer = @typeInfo(@TypeOf(manypointer)).pointer;
    if (pointer.sentinel_ptr) |_| {
        const len = std.mem.len(manypointer);
        return make_slice(manypointer[0..len], opts);
    } else {
        @compileError("it's not possible to create a manypointer without a sentinel");
    }
}

fn make_cpointer(cpointer: anytype, opts: anytype) beam.term {
    const pointer = @typeInfo(@TypeOf(cpointer)).pointer;
    const Child = pointer.child;

    if (@hasField(@TypeOf(opts), "length")) {
        return make(cpointer[0..options.length(opts).?], opts);
    } else {
        if (cpointer) |_| {
            // the following two types have inferrable sentinels
            if (Child == u8) {
                return make(@as([*:0]u8, @ptrCast(cpointer)), opts);
            }
            if (@typeInfo(Child) == .pointer) {
                return make(@as([*:null]Child, @ptrCast(cpointer)), opts);
            }

            switch (@typeInfo(Child)) {
                .@"struct" => return make(@as(*Child, @ptrCast(cpointer)), opts),
                else => @compileError("this is not supported"),
            }
        } else {
            return make(.nil, .{ .env = options.env(opts) });
        }
    }
}

pub fn make_slice(slice: anytype, opts: anytype) beam.term {
    const T = @TypeOf(slice);
    const slice_info = @typeInfo(T).pointer;
    const Child = slice_info.child;

    // u8 slices default to binary and must be opt-in to get charlists out.
    if (Child == u8 and options.output(opts) != .list) {
        return make_binary(slice, opts);
    }

    if (options.output(opts) == .binary) {
        return make_binary(slice, opts);
    }

    const env = options.env(opts);
    switch (T) {
        []beam.term => {
            // since beam.term is guaranteed to be a packed struct of only
            // e.ErlNifTerm, this is always guaranteed to work.
            const ptr = @as([*]const e.ErlNifTerm, @ptrCast(slice.ptr));
            return .{ .v = e.enif_make_list_from_array(env, ptr, @as(c_uint, @intCast(slice.len))) };
        },
        []e.ErlNifTerm => {
            return .{ .v = e.enif_make_list_from_array(env, slice.ptr, @as(c_uint, @intCast(slice.len))) };
        },
        else => {
            const list_child_as = if (@hasField(@TypeOf(opts), "as")) options.list_child(opts.as) else .default;
            const child_opts = .{ .env = env, .as = list_child_as };

            var tail = e.enif_make_list_from_array(env, null, 0);

            if (slice.len != 0) {
                var index = slice.len;
                while (index > 0) : (index -= 1) {
                    tail = e.enif_make_list_cell(env, make(slice[index - 1], child_opts).v, tail);
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
        .pointer => |P| {
            const Child = P.child;
            switch (P.size) {
                .slice => {
                    const binary_size = @sizeOf(Child) * content.len;
                    const u8buf = @as([*]const u8, @ptrCast(content.ptr));
                    return make_binary_from_u8_slice(u8buf[0..binary_size], opts);
                },
                // it is possible that this is a const pointer to an array in memory.
                .one => {
                    if (@typeInfo(Child) != .array) {
                        @compileError("make_binary is only supported for array and slice pointers");
                    }
                    const binary_size = @sizeOf(@typeInfo(Child).array.child) * content.len;
                    const u8buf = @as([*]const u8, @ptrCast(content));
                    return make_binary_from_u8_slice(u8buf[0..binary_size], opts);
                },
                else => @compileError("make_binary is only supported for array and slice pointers"),
            }
        },
        .array => |A| {
            const binary_size = @sizeOf(A.child) * content.len;
            const u8buf = @as([*]const u8, @ptrCast(&content));
            return make_binary_from_u8_slice(u8buf[0..binary_size], opts);
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

pub fn make_list_cell(head: anytype, tail: beam.term, opts: anytype) beam.term {
    return .{ .v = e.enif_make_list_cell(options.env(opts), make(head, opts).v, tail.v) };
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
