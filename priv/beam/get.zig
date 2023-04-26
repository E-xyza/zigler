const beam = @import("beam.zig");
const e = @import("erl_nif.zig");
const std = @import("std");
const resource = @import("resource.zig");

const GetError = error{ argument_error, unreachable_error };

fn allocator(opts: anytype) std.mem.Allocator {
    const T = @TypeOf(opts);
    if (@hasField(T, "allocator")) {
        return opts.allocator;
    }
    return beam.allocator;
}

pub fn get(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    // passthrough on beam.term and e.ErlNifTerm, no work needed.
    if (T == beam.term) return src;
    if (T == e.ErlNifTerm) return src.v;

    switch (@typeInfo(T)) {
        .Int => return get_int(T, env, src, opts),
        .Enum => return get_enum(T, env, src, opts),
        .Float => return get_float(T, env, src, opts),
        .Struct => return get_struct(T, env, src, opts),
        .Bool => return get_bool(T, env, src, opts),
        .Array => return get_array(T, env, src, opts),
        .Pointer => return get_pointer(T, env, src, opts),
        .Optional => return get_optional(T, env, src, opts),
        else => @compileError("unhandlable type encountered in get"),
    }
}

const c_int_size = @bitSizeOf(c_int);
const c_long_size = @bitSizeOf(c_long);
const i32_t = if (c_int_size == 32) c_int else if (c_long_size == 32) c_long;
const enif_get_i32 = if (c_int_size == 32) e.enif_get_int else if (c_long_size == 32) e.enif_get_long;
const u32_t = if (c_int_size == 32) c_uint else if (c_long_size == 32) c_ulong;
const enif_get_u32 = if (c_int_size == 32) e.enif_get_uint else if (c_long_size == 32) e.enif_get_ulong;

const minInt = std.math.minInt;
const maxInt = std.math.maxInt;

pub fn get_int(comptime T: type, env: beam.env, src: beam.term, opts: anytype) GetError!T {
    const int = @typeInfo(T).Int;
    switch (int.signedness) {
        .signed => switch (int.bits) {
            0...32 => {
                var result: i32_t = 0;
                try genericGetInt(T, env, src, &result, opts, e.enif_get_int);
                return lowerInt(T, env, src, result, opts);
            },
            33...64 => {
                var result: i64 = 0;
                try genericGetInt(T, env, src, &result, opts, e.enif_get_int64);
                return lowerInt(T, env, src, result, opts);
            },
            else => {
                // for integers bigger than 64-bytes the number
                // is imported as a binary.
                const Bigger = std.meta.Int(.unsigned, comptime try std.math.ceilPowerOfTwo(u16, int.bits));
                const bytes = @sizeOf(Bigger);

                var result: e.ErlNifBinary = undefined;

                // This should fail if it's not a binary.  Note there isn't much we can do here because
                // it is *supposed* to be marshalled into the nif.
                if (e.enif_inspect_binary(env, src.v, &result) == 0) return GetError.unreachable_error;

                var buf: Bigger = 0;
                std.mem.copy(u8, @ptrCast([*]u8, &buf)[0..bytes], result.data[0..bytes]);
                // check to make sure that the top bits are all zeros.
                const top_bit_count = (bytes * 8 - int.bits);
                if (@clz(buf) < top_bit_count) return GetError.argument_error;

                return @intCast(T, buf);
            },
        },
        .unsigned => switch (int.bits) {
            0...32 => {
                var result: u32_t = 0;
                try genericGetInt(T, env, src, &result, opts, e.enif_get_uint);
                return try lowerInt(T, env, src, result, opts);
            },
            33...64 => {
                var result: u64 = 0;
                try genericGetInt(T, env, src, &result, opts, e.enif_get_uint64);
                return try lowerInt(T, env, src, result, opts);
            },
            else => {
                // for integers bigger than 64-bytes the number
                // is imported as a binary.
                const Bigger = std.meta.Int(.unsigned, comptime try std.math.ceilPowerOfTwo(u16, int.bits));
                const bytes = @sizeOf(Bigger);

                var result: e.ErlNifBinary = undefined;

                // This should fail if it's not a binary.  Note there isn't much we can do here because
                // it is *supposed* to be marshalled into the nif.
                if (e.enif_inspect_binary(env, src.v, &result) == 0) return GetError.unreachable_error;

                var buf: Bigger = 0;
                std.mem.copy(u8, @ptrCast([*]u8, &buf)[0..bytes], result.data[0..bytes]);
                // check to make sure that the top bits are all zeros.
                const top_bit_count = (bytes * 8 - int.bits);
                if (@clz(buf) < top_bit_count) return GetError.argument_error;

                return @intCast(T, buf);
            },
        },
    }
}

inline fn genericGetInt(comptime T: type, env: beam.env, src: beam.term, result_ptr: anytype, opts: anytype, fun: anytype) GetError!void {
    errdefer error_expected(T, env, opts);
    errdefer error_got(env, opts, src);

    if (src.term_type(env) != .integer) {
        return GetError.argument_error;
    }

    if (fun(env, src.v, result_ptr) == 0) {
        error_line(env, opts, .{ "note: out of bounds (", .{ .inspect, minInt(T) }, "..", .{ .inspect, maxInt(T) }, ")" });
        return GetError.argument_error;
    }
}

inline fn lowerInt(comptime T: type, env: beam.env, src: beam.term, result: anytype, opts: anytype) GetError!T {
    errdefer error_expected(T, env, opts);
    errdefer error_got(env, opts, src);
    errdefer error_line(env, opts, .{ "note: out of bounds (", .{ .inspect, minInt(T) }, "..", .{ .inspect, maxInt(T) }, ")" });

    const int = @typeInfo(T).Int;
    if (int.signedness == .signed) {
        if (result < std.math.minInt(T)) {
            return GetError.argument_error;
        }
    }

    if (result > std.math.maxInt(T)) {
        return GetError.argument_error;
    }

    return @intCast(T, result);
}

pub fn get_enum(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    const enum_info = @typeInfo(T).Enum;
    const IntType = enum_info.tag_type;
    comptime var int_values: [enum_info.fields.len]IntType = undefined;
    comptime for (int_values) |*value, index| {
        value.* = enum_info.fields[index].value;
    };
    const enum_values = std.enums.values(T);

    errdefer error_expected(T, env, opts);
    errdefer error_got(env, opts, src);

    // prefer the integer form, fallback to string searches.
    switch (src.term_type(env)) {
        .integer => {
            errdefer error_line(env, opts, .{ "note: not an integer value for ", .{ .typename, @typeName(T) }, " (should be one of `", .{ .inspect, int_values }, "`)" });

            // put erasure on get_int setting the error_line
            const result = try get_int(IntType, env, src, .{});
            return try std.meta.intToEnum(T, result);
        },
        .atom => {
            errdefer error_line(env, opts, .{ "note: not an atom value for ", .{ .typename, @typeName(T) }, " (should be one of `", .{ .inspect, enum_values }, "`)" });

            // atoms cannot be longer than 256 characters.
            var buf: [256]u8 = undefined;
            const slice = try get_atom(env, src, &buf);

            inline for (enum_info.fields) |field| {
                if (std.mem.eql(u8, field.name[0..], slice)) return @field(T, field.name);
            }
            return GetError.argument_error;
        },
        else => return GetError.argument_error,
    }
}

const FloatAtoms = enum { infinity, neg_infinity, NaN };

pub fn get_float(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    // all floats in the beam are f64 types so this is relatively easy.
    errdefer error_expected(T, env, opts);
    errdefer error_got(env, opts, src);

    switch (src.term_type(env)) {
        .float => {
            var float: f64 = undefined;

            // this is not failable.
            _ = e.enif_get_double(env, src.v, &float);

            return @floatCast(T, float);
        },
        .atom => {
            // erase the errors coming back from get_enum!
            const special_form = get_enum(FloatAtoms, env, src, .{}) catch {
                error_line(env, opts, .{"note: not an atom value for f64 (should be one of `[:infinity, :neg_infinity, :NaN]`"});
                return GetError.argument_error;
            };

            return switch (special_form) {
                .infinity => std.math.inf(T),
                .neg_infinity => -std.math.inf(T),
                .NaN => std.math.nan(T),
            };
        },
        .integer => {
            error_line(env, opts, .{"note: integers are not allowed as arguments to float"});
            return GetError.argument_error;
        },
        else => {
            return GetError.argument_error;
        },
    }
}

pub fn get_atom(env: beam.env, src: beam.term, buf: *[256]u8) ![]u8 {
    const len = @intCast(usize, e.enif_get_atom(env, src.v, buf, 256, e.ERL_NIF_LATIN1));
    if (len == 0) return GetError.argument_error;
    return buf[0 .. len - 1];
}

pub fn get_struct(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    const struct_info = switch (@typeInfo(T)) {
        .Struct => |s| s,
        else => unreachable,
    };

    if (resource.MaybeUnwrap(struct_info)) |_| {
        return get_resource(T, env, src, opts);
    } else {
        errdefer error_expected(T, env, opts);
        errdefer error_got(env, opts, src);

        var result: T = undefined;
        try fill_struct(T, env, &result, src, opts);
        return result;
    }
}

pub fn get_resource(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    if (!@hasField(@TypeOf(opts), "root")) {
        @compileError("get must be provided a root option to obtain a resource, you can use @import(\"root\").");
    }

    errdefer error_expected(T, env, opts);
    errdefer error_got(env, opts, src);

    var res: T = undefined;
    var resource_type: *e.ErlNifResourceType = undefined;

    opts.root.assign_resource_type(resource.Unwrap(T), &resource_type);
    const resource_target = @ptrCast([*c]?*anyopaque, &res.resource_payload);
    const result = e.enif_get_resource(env, src.v, resource_type, resource_target);

    if (result == 0) return GetError.argument_error;
    return res;
}

// internal function, for getting individual tuples out of a keyword list for
// the purposes of filling out maplike data structures, e.g. `struct`
fn get_tuple_to_buf(env: beam.env, src: beam.term, buf: anytype) !void {
    // compile-time type checking on the buf variable
    const type_info = @typeInfo(@TypeOf(buf));
    const child_type_info = @typeInfo(type_info.Pointer.child);
    // compile-time type checking on the buf variable

    if (src.term_type(env) != .tuple) return GetError.argument_error;

    var arity: c_int = undefined;
    var src_array: [*c]e.ErlNifTerm = undefined;

    const result = e.enif_get_tuple(env, src.v, &arity, &src_array);

    if (result == 0) return GetError.argument_error;
    if (arity != child_type_info.Array.len) return GetError.argument_error;

    for (buf) |*slot, index| {
        slot.* = .{ .v = src_array[index] };
    }
}

pub fn get_bool(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    errdefer error_expected(T, env, opts);
    errdefer error_got(env, opts, src);

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

            error_line(env, opts, .{"note: only the atoms `true` and `false` are allowed to be bools"});
        },
        else => {},
    }

    return GetError.argument_error;
}

pub fn get_array(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    errdefer error_expected(T, env, opts);
    errdefer error_got(env, opts, src);

    var result: T = undefined;
    try fill_array(T, env, &result, src, opts);
    return result;
}

pub fn get_pointer(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    const pointer_info = @typeInfo(T).Pointer;
    const Child = pointer_info.child;
    switch (pointer_info.size) {
        .One => {
            const alloc = allocator(opts);
            var result = try alloc.create(Child);
            errdefer alloc.destroy(result);
            try fill(Child, env, result, src, opts);
            return result;
        },
        .Slice => {
            return get_slice(T, env, src, opts);
        },
        .Many => {
            return get_manypointer(T, env, src, opts);
        },
        .C => {
            return get_cpointer(T, env, src, opts);
        },
    }
}

pub fn get_optional(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    errdefer error_expected(T, env, opts);
    errdefer error_got(env, opts, src);

    const Child = @typeInfo(T).Optional.child;
    switch (src.term_type(env)) {
        .atom => return try null_or_error(T, env, src, opts),
        else => return try get(Child, env, src, opts),
    }
}

pub fn get_slice(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    errdefer error_expected(T, env, opts);
    errdefer error_got(env, opts, src);

    switch (src.term_type(env)) {
        .bitstring => return get_slice_binary(T, env, src, opts),
        .list => return get_slice_list(T, env, src, opts),
        else => return GetError.argument_error,
    }
}

pub fn get_slice_binary(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    const slice_info = @typeInfo(T).Pointer;
    const Child = slice_info.child;
    const child_info = @typeInfo(Child);
    // slices can be instantiated from binaries, for certain types of data.
    const bytes = switch (child_info) {
        // TODO: check that the argument errors here are correct.
        .Int => |i| if (i.bits % 8 != 0) return GetError.argument_error else i.bits / 8,
        .Float => |f| f.bits / 8,
        else => return GetError.argument_error,
    };
    const alloc = allocator(opts);

    var str_res: e.ErlNifBinary = undefined;
    if (e.enif_inspect_binary(env, src.v, &str_res) == 0) return GetError.unreachable_error;
    const item_count = str_res.size / bytes;
    const result_ptr = @ptrCast([*]Child, @alignCast(@alignOf(Child), str_res.data));

    if (slice_info.is_const) {
        return result_ptr[0..item_count];
    } else {
        var result = try alloc.alloc(Child, item_count);
        std.mem.copy(Child, result, result_ptr[0..item_count]);

        return result;
    }
}

pub fn get_slice_list(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    const Child = @typeInfo(T).Pointer.child;
    var length: c_uint = undefined;
    const alloc = allocator(opts);

    if (e.enif_get_list_length(env, src.v, &length) == 0) return GetError.unreachable_error;
    var result = try alloc.alloc(Child, length);
    errdefer alloc.free(result);

    var list: e.ErlNifTerm = src.v;
    for (result) |*item, index| {
        var head: e.ErlNifTerm = undefined;
        if (e.enif_get_list_cell(env, list, &head, &list) == 0) return GetError.unreachable_error;
        item.* = get(Child, env, .{ .v = head }, opts) catch |err| {
            if (err == GetError.argument_error) {
                error_enter(env, opts, .{ "at index ", .{ .inspect, index }, ":" });
            }
            return err;
        };
    }

    if (e.enif_is_empty_list(env, list) == 0) return GetError.unreachable_error;

    return result;
}

pub fn get_manypointer(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    // this is equivalent to creating a slice and then discarding the length term
    const Child = @typeInfo(T).Pointer.child;
    const slice = try get_slice([]Child, env, src, opts);
    const result = @ptrCast(T, slice.ptr);
    if (@typeInfo(T).Pointer.sentinel) |sentinel_ptr| {
        result[slice.len] = @ptrCast(*const Child, @alignCast(@alignOf(Child), sentinel_ptr)).*;
    }
    if (@hasField(@TypeOf(opts), "size")) {
        opts.size.* = slice.len;
    }
    return result;
}

pub fn get_cpointer(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    errdefer error_expected(T, env, opts);
    errdefer error_got(env, opts, src);

    const Child = @typeInfo(T).Pointer.child;
    // scan on the type of the source.
    switch (src.term_type(env)) {
        .atom => return try null_or_error(T, env, src, opts),
        .map => if (@typeInfo(Child) != .Struct) {
            return GetError.argument_error;
        } else {
            // we have to allocate this as a slice, so that it can be safely cleaned later.
            const alloc = allocator(opts);
            var result = try alloc.alloc(Child, 1);
            errdefer alloc.free(result);

            try fill(Child, env, &result[0], src, opts);

            if (@hasField(@TypeOf(opts), "size")) {
                opts.size.* = 1;
            }

            return result.ptr;
        },
        .list => {
            const result_slice = try get_slice_list([]Child, env, src, opts);
            if (@hasField(@TypeOf(opts), "size")) {
                opts.size.* = result_slice.len;
            }
            return result_slice.ptr;
        },
        .bitstring => {
            const result_slice = try get_slice_binary([]Child, env, src, opts);
            if (@hasField(@TypeOf(opts), "size")) {
                opts.size.* = result_slice.len;
            }
            return result_slice.ptr;
        },
        else => return GetError.argument_error,
    }
}

// fill functions
fn fill(comptime T: type, env: beam.env, result: *T, src: beam.term, opts: anytype) GetError!void {
    switch (@typeInfo(T)) {
        .Array => try fill_array(T, env, result, src, opts),
        .Struct => try fill_struct(T, env, result, src, opts),
        else => @compileError("unhandlable type encountered in fill"),
    }
}

fn fill_array(comptime T: type, env: beam.env, result: *T, src: beam.term, opts: anytype) GetError!void {
    const array_info = @typeInfo(T).Array;
    const Child = array_info.child;
    switch (src.term_type(env)) {
        .list => {
            // try to fill the array, if the lengths mismatch, then throw an error.
            // however, don't call enif_get_list_length because that incurs a second
            // pass through the array.
            var tail = src.v;
            for (result.*) |*item, index| {
                var head: e.ErlNifTerm = undefined;
                if (e.enif_get_list_cell(env, tail, &head, &tail) != 0) {
                    item.* = get(Child, env, .{ .v = head }, opts) catch |err| {
                        if (err == GetError.argument_error) {
                            error_enter(env, opts, .{ "at index ", .{ .inspect, index }, ":" });
                        }
                        return err;
                    };
                } else {
                    error_line(env, opts, .{ "note: length ", .{ .inspect, array_info.len }, " expected but got length ", .{ .inspect, index } });
                    return GetError.argument_error;
                }
            }
            if (e.enif_is_empty_list(env, tail) == 0) {
                var list_len: c_uint = undefined;
                if (e.enif_get_list_length(env, tail, &list_len) == 0) return GetError.unreachable_error;
                error_line(env, opts, .{ "note: length ", .{ .inspect, array_info.len }, " expected but got length ", .{ .inspect, list_len + array_info.len } });
                return GetError.argument_error;
            }
        },
        .bitstring => {
            // this code should never be executed, it only happens due to
            // semantic analysis.
            // TODO: check to make sure size-0 packed struct arrays are not supported.
            if (@alignOf(Child) == 0) unreachable;

            const expected_size = array_info.len * @sizeOf(Child);

            var str_res: e.ErlNifBinary = undefined;
            var u8_result_ptr = @ptrCast([*]u8, result);

            if (e.enif_inspect_binary(env, src.v, &str_res) == 0) return GetError.unreachable_error;

            if (str_res.size != expected_size) {
                error_line(env, opts, .{ "note: binary size ", .{ .inspect, expected_size }, " expected but got size ", .{ .inspect, str_res.size } });
                return GetError.argument_error;
            }

            std.mem.copy(u8, u8_result_ptr[0..str_res.size], str_res.data[0..str_res.size]);
        },
        else => return GetError.argument_error,
    }
}

fn fill_struct(comptime T: type, env: beam.env, result: *T, src: beam.term, opts: anytype) !void {
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
                    @field(result.*, field.name) = get(F, env, .{ .v = map_value }, opts) catch |err| {
                        if (err == GetError.argument_error) {
                            error_enter(env, opts, .{ "in field `:", field.name, "`:" });
                        }
                        return err;
                    };
                } else {
                    // note that this is a comptime if.
                    if (field.default_value) |default_value| {
                        @field(result.*, field.name) = @ptrCast(*const F, @alignCast(@alignOf(F), default_value)).*;
                    } else {
                        // can't return this directly due to compilation error.
                        failed = true;
                    }
                }

                if (failed) {
                    error_line(env, opts, .{ "note: ", .{ .typename, @typeName(T) }, " requires the field `:", field.name, "`, which is missing.)" });
                    return GetError.argument_error;
                }
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
                        @field(result.*, field.name) = get(field.field_type, env, value, opts) catch |err| {
                            if (err == GetError.argument_error) {
                                error_enter(env, opts, .{ "in field `:", field.name, "`:" });
                            }
                            return err;
                        };
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
                        error_line(env, opts, .{ "note: ", .{ .typename, @typeName(T) }, " requires the field `:", field.name, "`, which is missing.)" });
                        return GetError.argument_error;
                    }
                }
            }
        },
        .bitstring => {
            if (struct_info.layout == .Packed) {
                @compileError("not implemented yet");
            } else {
                return GetError.argument_error;
            }
        },
        else => return GetError.argument_error,
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

fn null_or_error(comptime T: type, env: beam.env, src: beam.term, opts: anytype) !T {
    errdefer error_line(env, opts, .{ "note: ", .{ .typename, @typeName(T) }, " can take the atom `nil` but no other atom" });

    var buf: [256]u8 = undefined;
    const atom = try get_atom(env, src, &buf);
    return if (std.mem.eql(u8, "nil", atom)) null else GetError.argument_error;
}

inline fn error_line(env: beam.env, opts: anytype, msg: anytype) void {
    // note that this function completely no-ops if the opts variable (which
    // in many cases is going to be `.{}`).  For the most part, this should be
    // used when detailed errors resulting from retrieving values from the VM
    // needs to be passed back to the VM.

    // in order to pass values back, .{} should contain the `.error_info` field
    // and this field should be a pointer to a `beam.term` object.  Anything
    // else will result in a compiler error.

    inline for (@typeInfo(@TypeOf(opts)).Struct.fields) |field| {
        if (std.mem.eql(u8, "error_info", field.name)) {
            const field_type = @TypeOf(opts.error_info);
            if (field_type != *beam.term) {
                const error_msg = comptime mblk: {
                    break :mblk std.fmt.comptimePrint("the `.error_info` field of the get opts parameter must be `*beam.term`, got: {}", .{field_type});
                };
                @compileError(error_msg);
            }
            opts.error_info.v = e.enif_make_list_cell(env, beam.make(env, msg, .{}).v, opts.error_info.v);
        }
    }
}

inline fn error_expected(comptime T: type, env: beam.env, opts: anytype) void {
    // it's not entirely obvious why, but this needs to be put into a comptime block
    // to avoid a memory aliasing bug that exists in ziglang. (0.10.0)
    const typespec = comptime ts: {
        break :ts typespec_for(T);
    };

    error_line(env, opts, .{ "expected: ", typespec, " (for `", .{ .typename, @typeName(T) }, "`)" });
}

inline fn error_got(env: beam.env, opts: anytype, src: beam.term) void {
    error_line(env, opts, .{ "got: `", .{ .inspect, src }, "`" });
}

inline fn error_enter(env: beam.env, opts: anytype, msg: anytype) void {
    error_line(env, opts, .enter);
    error_line(env, opts, msg);
}

fn typespec_for(comptime T: type) []const u8 {
    return switch (@typeInfo(T)) {
        .Int => "integer",
        .Enum => "integer | atom",
        .Float => "float | :infinity | :neg_infinity | :NaN",
        .Struct => |s| make_struct: {
            const maybe_binary = if (s.layout == .Packed) " | binary" else "";
            break :make_struct "map | keyword" ++ maybe_binary;
        },
        .Bool => "boolean",
        .Array => |a| maybe_array_term(a, @sizeOf(T)),
        .Pointer => |p| switch (p.size) {
            // pointer to one can only be a map or keyword.
            .One => "map | keyword",
            .Slice => maybe_binary_term(p),
            .Many => maybe_binary_term(p),
            .C => comptime make_cpointer: {
                const or_single = if (@typeInfo(p.child) == .Struct) "map | " else "";
                break :make_cpointer or_single ++ maybe_binary_term(p);
            },
        },
        .Optional => |o| comptime make_optional: {
            break :make_optional "nil | " ++ typespec_for(o.child);
        },
        else => @compileError("unreachable"),
    };
}

fn maybe_array_term(comptime term_info: anytype, comptime array_bytes: usize) []const u8 {
    const Child = term_info.child;
    const child_term_type = comptime btbrk: {
        break :btbrk "list(" ++ typespec_for(Child) ++ ")";
    };
    if (Child == u8) return "binary | " ++ child_term_type;
    return std.fmt.comptimePrint("<<_::binary-size({})>> | ", .{array_bytes}) ++ child_term_type;
}

fn maybe_binary_term(comptime term_info: anytype) []const u8 {
    const Child = term_info.child;
    const child_term_type = comptime btbrk: {
        break :btbrk "list(" ++ typespec_for(Child) ++ ")";
    };
    if (Child == u8) return "binary | " ++ child_term_type;
    const r = switch (@typeInfo(Child)) {
        .Int => |i| std.fmt.comptimePrint("<<_::_ * {}>> | ", .{i.bits}) ++ child_term_type,
        .Float => |f| std.fmt.comptimePrint("<<_::_ * {}>> | ", .{f.bits}) ++ child_term_type,
        else => child_term_type,
    };
    return r;
}
