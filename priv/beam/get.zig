const beam = @import("beam.zig");
const e = @import("erl_nif.zig");
const std = @import("std");

pub const GetError = error{ nif_argument_type_error, nif_argument_range_error, nif_struct_field_error, nif_struct_missing_field_error, nif_argument_enum_not_found_error, nif_array_length_error, nif_marshalling_error // this should really not happen.
};

pub fn get(comptime T: type, env: beam.env, src: beam.term) !T {
    // passthrough on beam.term and e.ErlNifTerm, no work needed.
    if (T == beam.term) return src;
    if (T == e.ErlNifTerm) return src.v;

    switch (@typeInfo(T)) {
        .Int => return get_int(T, env, src),
        .Enum => return get_enum(T, env, src),
        .Float => return get_float(T, env, src),
        .Struct => return get_struct(T, env, src),
        .Bool => return get_bool(T, env, src),
        .Array => return get_array(T, env, src),
        .Pointer => return get_pointer(T, env, src),
        .Optional => return get_optional(T, env, src),
        else => @compileError("unhandlable type encountered in get"),
    }
}

const c_int_size = @bitSizeOf(c_int);
const c_long_size = @bitSizeOf(c_long);
const i32_t = if (c_int_size == 32) c_int else if (c_long_size == 32) c_long;
const enif_get_i32 = if (c_int_size == 32) e.enif_get_int else if (c_long_size == 32) e.enif_get_long;
const u32_t = if (c_int_size == 32) c_uint else if (c_long_size == 32) c_ulong;
const enif_get_u32 = if (c_int_size == 32) e.enif_get_uint else if (c_long_size == 32) e.enif_get_ulong;

pub fn get_int(comptime T: type, env: beam.env, src: beam.term) GetError!T {
    const int = @typeInfo(T).Int;
    switch (int.signedness) {
        .signed => switch (int.bits) {
            0...32 => {
                var result: i32_t = 0;

                if (src.term_type(env) != .integer) {
                    return GetError.nif_argument_type_error;
                }

                if (enif_get_i32(env, src.v, &result) == 0) return GetError.nif_argument_range_error;

                return lowerInt(T, result);
            },
            33...64 => {
                var result: i64 = 0;

                if (src.term_type(env) != .integer) {
                    return GetError.nif_argument_type_error;
                }

                if (e.enif_get_int64(env, src.v, &result) == 0) return GetError.nif_argument_range_error;

                return lowerInt(T, result);
            },
            else => {
                // for integers bigger than 64-bytes the number
                // is imported as a binary.
                const Bigger = std.meta.Int(.unsigned, comptime try std.math.ceilPowerOfTwo(u16, int.bits));
                const bytes = @sizeOf(Bigger);

                var result: e.ErlNifBinary = undefined;

                // This should fail if it's not a binary.  Note there isn't much we can do here because
                // it is *supposed* to be marshalled into the nif.
                if (e.enif_inspect_binary(env, src.v, &result) == 0) return GetError.nif_marshalling_error;

                var buf: Bigger = 0;
                std.mem.copy(u8, @ptrCast([*]u8, &buf)[0..bytes], result.data[0..bytes]);
                // check to make sure that the top bits are all zeros.
                const top_bit_count = (bytes * 8 - int.bits);
                if (@clz(buf) < top_bit_count) return GetError.nif_argument_range_error;

                return @intCast(T, buf);
            },
        },
        .unsigned => switch (int.bits) {
            0...32 => {
                var result: u32_t = 0;

                if (src.term_type(env) != .integer) {
                    return GetError.nif_argument_type_error;
                }

                // TODO: check to make sure this doesn't get weird
                if (enif_get_u32(env, src.v, &result) == 0) return GetError.nif_argument_range_error;

                return try lowerInt(T, result);
            },
            33...64 => {
                var result: u64 = 0;

                if (src.term_type(env) != .integer) {
                    return GetError.nif_argument_type_error;
                }

                if (e.enif_get_uint64(env, src.v, &result) == 0) return GetError.nif_argument_range_error;

                return try lowerInt(T, result);
            },
            else => {
                // for integers bigger than 64-bytes the number
                // is imported as a binary.
                const Bigger = std.meta.Int(.unsigned, comptime try std.math.ceilPowerOfTwo(u16, int.bits));
                const bytes = @sizeOf(Bigger);

                var result: e.ErlNifBinary = undefined;

                // This should fail if it's not a binary.  Note there isn't much we can do here because
                // it is *supposed* to be marshalled into the nif.
                if (e.enif_inspect_binary(env, src.v, &result) == 0) return GetError.nif_marshalling_error;

                var buf: Bigger = 0;
                std.mem.copy(u8, @ptrCast([*]u8, &buf)[0..bytes], result.data[0..bytes]);
                // check to make sure that the top bits are all zeros.
                const top_bit_count = (bytes * 8 - int.bits);
                if (@clz(buf) < top_bit_count) return GetError.nif_argument_range_error;

                return @intCast(T, buf);
            },
        },
    }
    // for now, panic.
    unreachable;
}

inline fn lowerInt(comptime T: type, result: anytype) GetError!T {
    const int = @typeInfo(T).Int;
    if (int.signedness == .signed) {
        if (result < std.math.minInt(T)) {
            return GetError.nif_argument_range_error;
        }
    }

    if (result > std.math.maxInt(T)) {
        return GetError.nif_argument_range_error;
    }

    return @intCast(T, result);
}

pub fn get_enum(comptime T: type, env: beam.env, src: beam.term) !T {
    const enumInfo = @typeInfo(T).Enum;
    const IntType = enumInfo.tag_type;
    // prefer the integer form, fallback to string searches.
    switch (src.term_type(env)) {
        .integer => return @intToEnum(T, try get_int(IntType, env, src)),
        .atom => {
            // atoms cannot be longer than 256 characters.
            var buf: [256]u8 = undefined;
            const slice = try get_atom(env, src, &buf);

            inline for (enumInfo.fields) |field| {
                if (std.mem.eql(u8, field.name[0..], slice)) return @field(T, field.name);
            }
            return GetError.nif_argument_enum_not_found_error;
        },
        else => return GetError.nif_argument_type_error,
    }
}

const FloatAtoms = enum { infinity, neg_infinity, NaN };

pub fn get_float(comptime T: type, env: beam.env, src: beam.term) !T {
    // all floats in the beam are f64 types so this is relatively easy.
    switch (src.term_type(env)) {
        .float => {
            var float: f64 = undefined;

            // this is not failable.
            _ = e.enif_get_double(env, src.v, &float);

            return @floatCast(T, float);
        },
        .atom => {
            const special_form = get_enum(FloatAtoms, env, src) catch return GetError.nif_argument_type_error;
            return switch (special_form) {
                .infinity => std.math.inf(T),
                .neg_infinity => -std.math.inf(T),
                .NaN => std.math.nan(T),
            };
        },
        else => return GetError.nif_argument_type_error,
    }
}

pub fn get_atom(env: beam.env, src: beam.term, buf: *[256]u8) ![]u8 {
    const len = @intCast(usize, e.enif_get_atom(env, src.v, buf, 256, e.ERL_NIF_LATIN1));
    if (len == 0) return GetError.nif_argument_type_error;
    return buf[0 .. len - 1];
}

pub fn get_struct(comptime T: type, env: beam.env, src: beam.term) !T {
    var result: T = undefined;
    try fill_struct(T, env, &result, src);
    return result;
}

// internal function, for getting individual tuples out of a keyword list for
// the purposes of filling out maplike data structures, e.g. `struct`
fn get_tuple_to_buf(env: beam.env, src: beam.term, buf: anytype) !void {
    // compile-time type checking on the buf variable
    const type_info = @typeInfo(@TypeOf(buf));
    if (type_info != .Pointer) @compileError("get_tuple_to_buf buffer must be a pointer to an array of beam.term");
    if (type_info.Pointer.size != .One) @compileError("get_tuple_to_buf buffer must be a pointer to an array of beam.term");
    if (type_info.Pointer.sentinel != null) @compileError("get_tuple_to_buf buffer must be a pointer to an array of beam.term");
    if (type_info.Pointer.is_const) @compileError("get_tuple_to_buf buffer must be a pointer to an array of beam.term");
    const child_type_info = @typeInfo(type_info.Pointer.child);
    if (child_type_info != .Array) @compileError("get_tuple_to_buf buffer must be a pointer to an array of beam.term");
    if (child_type_info.Array.child != beam.term) @compileError("get_tuple_to_buf buffer must be a pointer to an array of beam.term");
    // compile-time type checking on the buf variable

    if (src.term_type(env) != .tuple) return GetError.nif_argument_type_error;

    var arity: c_int = undefined;
    var src_array: [*c]e.ErlNifTerm = undefined;

    const result = e.enif_get_tuple(env, src.v, &arity, &src_array);

    if (result == 0) return GetError.nif_argument_type_error;
    if (arity != child_type_info.Array.len) return GetError.nif_argument_type_error;

    for (buf) |*slot, index| {
        slot.* = .{ .v = src_array[index] };
    }
}

pub fn get_bool(comptime T: type, env: beam.env, src: beam.term) !bool {
    if (T != bool) @compileError("get_bool may only be called with the bool type");
    switch (src.term_type(env)) {
        .atom => {
            var buf: [256]u8 = undefined;
            const atom = try get_atom(env, src, &buf);
            if (std.mem.eql(u8, "true", atom)) {
                return true;
            }
            if (std.mem.eql(u8, "false", atom)) {
                return false;
            }
            return GetError.nif_argument_type_error;
        },
        else => return GetError.nif_argument_type_error,
    }
}

pub fn get_array(comptime T: type, env: beam.env, src: beam.term) !T {
    var result: T = undefined;
    try fill_array(T, env, &result, src);
    return result;
}

pub fn get_pointer(comptime T: type, env: beam.env, src: beam.term) !T {
    // note that CALLER of get has the responsibility of freeing the result, if it
    // has succeeded.

    const pointer_info = @typeInfo(T).Pointer;
    const Child = pointer_info.child;
    switch (pointer_info.size) {
        .One => {
            var result = try beam.allocator.create(Child);
            errdefer beam.allocator.destroy(result);
            try fill(Child, env, result, src);
            return result;
        },
        .Slice => {
            return get_slice(T, env, src);
        },
        .Many => {
            return get_manypointer(T, env, src);
        },
        .C => {
            return get_cpointer(T, env, src);
        },
    }
}

pub fn get_optional(comptime T: type, env: beam.env, src: beam.term) !T {
    const Child = @typeInfo(T).Optional.child;

    switch (src.term_type(env)) {
        .atom => return try null_or_error(env, src),
        else => return try get(Child, env, src),
    }
}

pub fn get_slice(comptime T: type, env: beam.env, src: beam.term) !T {
    switch (src.term_type(env)) {
        .bitstring => return get_slice_binary(T, env, src),
        .list => return get_slice_list(T, env, src),
        else => return GetError.nif_argument_type_error,
    }
}

pub fn get_slice_binary(comptime T: type, env: beam.env, src: beam.term) !T {
    const slice_info = @typeInfo(T).Pointer;
    // slices can be instantiated from binaries, if they are u8 arrays
    if (slice_info.child != u8) return GetError.nif_argument_type_error;
    var str_res: e.ErlNifBinary = undefined;
    if (e.enif_inspect_binary(env, src.v, &str_res) == 0) return GetError.nif_marshalling_error;
    var result = try beam.allocator.alloc(u8, str_res.size);
    std.mem.copy(u8, result, str_res.data[0..result.len]);
    return result;
}

pub fn get_slice_list(comptime T: type, env: beam.env, src: beam.term) !T {
    const Child = @typeInfo(T).Pointer.child;
    var length: c_uint = undefined;

    if (e.enif_get_list_length(env, src.v, &length) == 0) return GetError.nif_marshalling_error;
    var result = try beam.allocator.alloc(Child, length);
    errdefer beam.allocator.free(result);

    var list: e.ErlNifTerm = src.v;
    for (result) |*item| {
        var head: e.ErlNifTerm = undefined;
        if (e.enif_get_list_cell(env, list, &head, &list) == 0) return GetError.nif_marshalling_error;
        item.* = try get(Child, env, .{ .v = head });
    }

    if (e.enif_is_empty_list(env, list) == 0) return GetError.nif_marshalling_error;

    return result;
}

pub fn get_manypointer(comptime T: type, env: beam.env, src: beam.term) !T {
    // this is equivalent to creating a slice and then discarding the length term
    const Child = @typeInfo(T).Pointer.child;
    const slice = try get_slice([]Child, env, src);
    const result = @ptrCast(T, slice.ptr);
    if (@typeInfo(T).Pointer.sentinel) |sentinel_ptr| {
        result[slice.len] = @ptrCast(*const Child, @alignCast(@alignOf(Child), sentinel_ptr)).*;
    }
    return result;
}

pub fn get_cpointer(comptime T: type, env: beam.env, src: beam.term) !T {
    const Child = @typeInfo(T).Pointer.child;
    // scan on the type of the source.
    return switch (src.term_type(env)) {
        .atom => try null_or_error(T, env, src),
        .map => 
            if (@typeInfo(Child) != .Struct) 
                GetError.nif_argument_type_error 
            else 
                try get_pointer(*Child, env, src),
        .list =>
            (try get_slice_list([]Child, env, src)).ptr,
        .bitstring =>
            (try get_slice_binary([]Child, env, src)).ptr,
        else => GetError.nif_argument_type_error,
    };
}

// fill functions
fn fill(comptime T: type, env: beam.env, result: *T, src: beam.term) GetError!void {
    switch (@typeInfo(T)) {
        .Array => try fill_array(T, env, result, src),
        .Struct => try fill_struct(T, env, result, src),
        else => @compileError("unhandlable type encountered in fill"),
    }
}

fn fill_array(comptime T: type, env: beam.env, result: *T, src: beam.term) GetError!void {
    const array_info = @typeInfo(T).Array;
    const Child = array_info.child;
    switch (src.term_type(env)) {
        .list => {
            // try to fill the array, if the lengths mismatch, then throw an error.
            // however, don't call enif_get_list_length because that incurs a second
            // pass through the array.
            var tail = src.v;
            for (result.*) |*item| {
                var head: e.ErlNifTerm = undefined;
                if (e.enif_get_list_cell(env, tail, &head, &tail) != 0) {
                    item.* = try get(Child, env, .{ .v = head });
                } else return GetError.nif_array_length_error;
            }
            if (e.enif_is_empty_list(env, tail) == 0) return GetError.nif_array_length_error;
        },
        .bitstring => {
            // arrays can be instantiated as binaries, if they are u8 arrays
            if (Child != u8) return GetError.nif_argument_type_error;

            var str_res: e.ErlNifBinary = undefined;

            if (e.enif_inspect_binary(env, src.v, &str_res) == 0) return GetError.nif_marshalling_error;

            if (str_res.size != array_info.len) return GetError.nif_array_length_error;

            std.mem.copy(u8, result, str_res.data[0..array_info.len]);
        },
        else => return GetError.nif_argument_type_error,
    }
}

fn fill_struct(comptime T: type, env: beam.env, result: *T, src: beam.term) GetError!void {
    const struct_info = @typeInfo(T).Struct;
    switch (src.term_type(env)) {
        .map => {
            var failed: bool = false;
            // look for each of the fields:
            inline for (struct_info.fields) |field| {
                const F = field.field_type;
                const field_atom = beam.make_into_atom(env, field.name);
                var map_value: e.ErlNifTerm = undefined;
                if (e.enif_get_map_value(env, src.v, field_atom.v, &map_value) == 1) {
                    @field(result.*, field.name) = get(F, env, .{ .v = map_value }) catch return GetError.nif_struct_field_error;
                } else {
                    // note that this is a comptime if.
                    if (field.default_value) |default_value| {
                        @field(result.*, field.name) = @ptrCast(*const F, @alignCast(@alignOf(F), default_value)).*;
                    } else {
                        // can't return this directly due to compilation error.
                        failed = true;
                    }
                }

                if (failed) return GetError.nif_struct_missing_field_error;
            }
        },
        .list => {
            var head: e.ErlNifTerm = undefined;
            var tail: e.ErlNifTerm = undefined;
            var list: e.ErlNifTerm = src.v;
            var tuple_buf: [2]beam.term = undefined;
            var atom_buf: [256]u8 = undefined;
            var registry: StructRegistry(T) = .{};

            while (e.enif_get_list_cell(env, list, &head, &tail) == 1) : (list = tail) {
                var item: beam.term = .{ .v = head };
                try get_tuple_to_buf(env, item, &tuple_buf);
                const key = tuple_buf[0];
                const value = tuple_buf[1];
                const atom_name = try get_atom(env, key, &atom_buf);

                // scan the list of fields to see if we have found one.
                scan_fields: inline for (struct_info.fields) |field| {
                    if (std.mem.eql(u8, atom_name, field.name)) {
                        @field(result.*, field.name) = get(field.field_type, env, value) catch return GetError.nif_struct_field_error;
                        // label the registry as complete.
                        @field(registry, field.name) = true;
                        break :scan_fields;
                    }
                }
            }

            inline for (struct_info.fields) |field| {
                // skip anything that was defined in the last section.
                if (!@field(registry, field.name)) {
                    const Tf = field.field_type;
                    if (field.default_value) |defaultptr| {
                        @field(result.*, field.name) = @ptrCast(*const Tf, @alignCast(@alignOf(Tf), defaultptr)).*;
                    } else {
                        return GetError.nif_struct_missing_field_error;
                    }
                }
            }
        },
        .bitstring => {
            if (struct_info.layout == .Packed) {
                // the bitstring is going to be *padded*, so we need to take this into account.
                //var str_res: e.ErlNifBinary = undefined;
                ////const bytes = bytesFor(T);
                //const buf = @ptrToInt(result);

                //std.debug.print("yooo {}\n", .{buf});
                //_ = buf;

                // This should fail if it's not a binary.  Note there isn't much we can do here because
                // it is *supposed* to be marshalled into the nif.
                //if (e.enif_inspect_binary(env, src.v, &str_res) == 0) return GetError.nif_marshalling_error;

                @panic("whoops");

                //std.mem.copy(u8, buf, str_res.data[0..buf.len]);
            } else {
                return GetError.nif_argument_type_error;
            }
        },
        else => return GetError.nif_argument_type_error,
    }
}

pub fn StructRegistry(comptime SourceStruct: type) type {
    const source_info = @typeInfo(SourceStruct);
    if (source_info != .Struct) @compileError("StructRegistry may only be called with a struct type");
    const source_fields = source_info.Struct.fields;
    const default = false;

    var fields: [source_fields.len]std.builtin.Type.StructField = undefined;

    for (source_fields) |source_field, index| {
        fields[index] = .{ .name = source_field.name, .field_type = bool, .default_value = &default, .is_comptime = false, .alignment = @alignOf(*bool) };
    }

    const decls = [0]std.builtin.Type.Declaration{};
    const constructed_struct = std.builtin.Type.Struct{
        .layout = .Auto,
        .fields = fields[0..],
        .decls = decls[0..],
        .is_tuple = false,
    };

    return @Type(.{ .Struct = constructed_struct });
}

fn bytesFor(comptime T: type) comptime_int {
    const bitsize = @bitSizeOf(T);
    return bitsize / 8 + if (bitsize % 8 == 0) 0 else 1;
}

// there's probably a std function for this.
fn IntFor(comptime bits: comptime_int) type {
    return @Type(.{ .Int = .{ .signedness = .unsigned, .bits = bits } });
}

fn null_or_error(comptime T: type, env: beam.env, src: beam.term) !T {
    var buf: [256]u8 = undefined;
    const atom = try get_atom(env, src, &buf);
    return if (std.mem.eql(u8, "nil", atom)) null else GetError.nif_argument_type_error;
}
