const beam = @import("beam.zig");
const e = @import("erl_nif");
const std = @import("std");
const resource = @import("resource.zig");
const options = @import("options.zig");

const GetError = error{ badarg, unreachable_error };

// get API function.

pub fn get(comptime T: type, src: beam.term, opts: anytype) !T {
    // passthrough on beam.term and e.ErlNifTerm, no work needed.

    if (T == beam.term) return src;
    if (T == beam.pid) return get_pid(src, opts);
    if (T == e.ErlNifTerm) return src.v;

    switch (@typeInfo(T)) {
        .int => return get_int(T, src, opts),
        .@"enum" => return get_enum(T, src, opts),
        .float => return get_float(T, src, opts),
        .@"struct" => return get_struct(T, src, opts),
        .bool => return get_bool(T, src, opts),
        .array => return get_array(T, src, opts),
        .pointer => return get_pointer(T, src, opts),
        .optional => return get_optional(T, src, opts),
        else => @compileError("unhandlable type encountered in get"),
    }
}

// basic special types
pub fn get_pid(src: beam.term, opts: anytype) GetError!beam.pid {
    var pid: beam.pid = undefined;
    if (e.enif_get_local_pid(options.env(opts), src.v, &pid) == 0) return GetError.badarg;
    return pid;
}

pub fn get_port(src: beam.term, opts: anytype) GetError!beam.port {
    var port: beam.port = undefined;
    if (e.enif_get_local_port(options.env(opts), src.v, &port) == 0) return GetError.badarg;
    return port;
}

const c_int_size = @bitSizeOf(c_int);
const c_long_size = @bitSizeOf(c_long);
const i32_t = if (c_int_size == 32) c_int else if (c_long_size == 32) c_long;
const enif_get_i32 = if (c_int_size == 32) e.enif_get_int else if (c_long_size == 32) e.enif_get_long;
const u32_t = if (c_int_size == 32) c_uint else if (c_long_size == 32) c_ulong;
const enif_get_u32 = if (c_int_size == 32) e.enif_get_uint else if (c_long_size == 32) e.enif_get_ulong;

const minInt = std.math.minInt;
const maxInt = std.math.maxInt;

pub fn get_int(comptime T: type, src: beam.term, opts: anytype) GetError!T {
    const int = @typeInfo(T).int;
    switch (int.signedness) {
        .signed => switch (int.bits) {
            0...32 => {
                var result: i32_t = 0;
                try genericGetInt(T, src, &result, opts, e.enif_get_int);
                return lowerInt(T, src, result, opts);
            },
            33...64 => {
                var result: i64 = 0;
                try genericGetInt(T, src, &result, opts, e.enif_get_int64);
                return lowerInt(T, src, result, opts);
            },
            else => {
                // for integers bigger than 64-bytes the number
                // is imported as a binary.
                const Bigger = std.meta.Int(.unsigned, comptime try std.math.ceilPowerOfTwo(u16, int.bits));
                const bytes = @sizeOf(Bigger);

                var result: e.ErlNifBinary = undefined;

                // This should fail if it's not a binary.  Note there isn't much we can do here because
                // it is *supposed* to be marshalled into the nif.
                if (e.enif_inspect_binary(options.env(opts), src.v, &result) == 0) return GetError.unreachable_error;

                var buf: Bigger = 0;
                const buf_ptr: [*]u8 = @ptrCast(&buf);
                @memcpy(buf_ptr[0..bytes], result.data[0..bytes]);
                // check to make sure that the top bits are all zeros.
                const top_bit_count = (bytes * 8 - int.bits);
                if (@clz(buf) < top_bit_count) return GetError.badarg;

                return @as(T, @intCast(buf));
            },
        },
        .unsigned => switch (int.bits) {
            0...32 => {
                var result: u32_t = 0;
                try genericGetInt(T, src, &result, opts, e.enif_get_uint);
                return try lowerInt(T, src, result, opts);
            },
            33...64 => {
                var result: u64 = 0;
                try genericGetInt(T, src, &result, opts, e.enif_get_uint64);
                return try lowerInt(T, src, result, opts);
            },
            else => {
                // for integers bigger than 64-bytes the number
                // is imported as a binary.
                const Bigger = std.meta.Int(.unsigned, comptime try std.math.ceilPowerOfTwo(u16, int.bits));
                const bytes = @sizeOf(Bigger);

                var result: e.ErlNifBinary = undefined;

                // This should fail if it's not a binary.  Note there isn't much we can do here because
                // it is *supposed* to be marshalled into the nif.
                if (e.enif_inspect_binary(options.env(opts), src.v, &result) == 0) return GetError.unreachable_error;

                var buf: Bigger = 0;
                @memcpy(@as([*]u8, @ptrCast(&buf))[0..bytes], result.data[0..bytes]);
                // check to make sure that the top bits are all zeros.
                const top_bit_count = (bytes * 8 - int.bits);
                if (@clz(buf) < top_bit_count) return GetError.badarg;

                return @as(T, @intCast(buf));
            },
        },
    }
}

inline fn genericGetInt(comptime T: type, src: beam.term, result_ptr: anytype, opts: anytype, fun: anytype) GetError!void {
    errdefer error_expected(T, opts);
    errdefer error_got(src, opts);

    if (src.term_type(opts) != .integer) {
        return GetError.badarg;
    }

    if (fun(options.env(opts), src.v, result_ptr) == 0) {
        error_line(.{ "note: out of bounds (", .{ .inspect, minInt(T) }, "..", .{ .inspect, maxInt(T) }, ")" }, opts);
        return GetError.badarg;
    }
}

inline fn lowerInt(comptime T: type, src: beam.term, result: anytype, opts: anytype) GetError!T {
    errdefer error_expected(T, opts);
    errdefer error_got(src, opts);
    errdefer error_line(.{ "note: out of bounds (", .{ .inspect, minInt(T) }, "..", .{ .inspect, maxInt(T) }, ")" }, opts);

    const int = @typeInfo(T).int;
    if (int.signedness == .signed) {
        if (result < std.math.minInt(T)) {
            return GetError.badarg;
        }
    }

    if (result > std.math.maxInt(T)) {
        return GetError.badarg;
    }

    return @as(T, @intCast(result));
}

pub fn get_enum(comptime T: type, src: beam.term, opts: anytype) !T {
    const enum_info = @typeInfo(T).@"enum";
    const IntType = enum_info.tag_type;
    comptime var int_values: [enum_info.fields.len]IntType = undefined;
    comptime for (&int_values, 0..) |*value, index| {
        value.* = enum_info.fields[index].value;
    };
    const enum_values = std.enums.values(T);

    error_got(src, opts);
    error_expected(T, opts);

    // prefer the integer form, fallback to string searches.
    switch (src.term_type(opts)) {
        .integer => {
            errdefer error_line(.{ "note: not an integer value for ", .{ .typename, @typeName(T) }, " (should be one of `", .{ .inspect, int_values }, "`)" }, opts);

            // put erasure on get_int setting the error_line
            const result = try get_int(IntType, src, opts);
            return try std.meta.intToEnum(T, result);
        },
        .atom => {
            errdefer error_line(.{ "note: not an atom value for ", .{ .typename, @typeName(T) }, " (should be one of `", .{ .inspect, enum_values }, "`)" }, opts);

            // atoms cannot be longer than 256 characters.
            var buf: [256]u8 = undefined;
            const slice = try get_atom(src, &buf, opts);

            inline for (enum_info.fields) |field| {
                if (std.mem.eql(u8, field.name[0..], slice)) return @field(T, field.name);
            }
            return GetError.badarg;
        },
        else => return GetError.badarg,
    }
}

const FloatAtoms = enum { infinity, neg_infinity, NaN };

pub fn get_float(comptime T: type, src: beam.term, opts: anytype) !T {
    // all floats in the beam are f64 types so this is relatively easy.
    errdefer error_expected(T, opts);
    errdefer error_got(src, opts);

    switch (src.term_type(opts)) {
        .float => {
            var float: f64 = undefined;

            // this is not failable.
            _ = e.enif_get_double(options.env(opts), src.v, &float);

            return @as(T, @floatCast(float));
        },
        .atom => {
            // erase the errors coming back from get_enum!
            const special_form = get_enum(FloatAtoms, src, .{}) catch {
                error_line(.{ "note: not an atom value for ", .{ .typename, @typeName(T) }, " (should be one of `[:infinity, :neg_infinity, :NaN]`" }, opts);
                return GetError.badarg;
            };

            return switch (special_form) {
                .infinity => std.math.inf(T),
                .neg_infinity => -std.math.inf(T),
                .NaN => std.math.nan(T),
            };
        },
        .integer => {
            error_line(.{"note: integers are not allowed as arguments to float"}, opts);
            return GetError.badarg;
        },
        else => {
            return GetError.badarg;
        },
    }
}

pub fn get_atom(src: beam.term, buf: *[256]u8, opts: anytype) ![]u8 {
    const len = @as(usize, @intCast(e.enif_get_atom(options.env(opts), src.v, buf, 256, e.ERL_NIF_LATIN1)));
    if (len == 0) return GetError.badarg;
    return buf[0 .. len - 1];
}

pub fn get_struct(comptime T: type, src: beam.term, opts: anytype) !T {
    const struct_info = switch (@typeInfo(T)) {
        .@"struct" => |s| s,
        else => unreachable,
    };

    if (resource.MaybeUnwrap(struct_info)) |_| {
        return get_resource(T, src, opts);
    } else {
        errdefer error_expected(T, opts);
        errdefer error_got(src, opts);

        var result: T = undefined;
        try fill_struct(T, &result, src, opts);
        return result;
    }
}

pub fn get_resource(comptime T: type, src: beam.term, opts: anytype) !T {
    errdefer error_expected(T, opts);
    errdefer error_got(src, opts);

    // make sure it's a reference type
    if (src.term_type(opts) != .ref) {
        return GetError.badarg;
    }

    var res: T = undefined;
    res.get(src, opts) catch {
        error_line(.{"note: the reference passed is not associated with a resource of the correct type"}, opts);
        return GetError.badarg;
    };

    // by default, we keep the resource.
    if (options.should_keep(opts)) {
        res.keep();
    }

    return res;
}

// internal function, for getting individual tuples out of a keyword list for
// the purposes of filling out maplike data structures, e.g. `struct`
fn get_tuple_to_buf(src: beam.term, buf: anytype, opts: anytype) !void {
    // compile-time type checking on the buf variable
    const type_info = @typeInfo(@TypeOf(buf));
    const child_type_info = @typeInfo(type_info.pointer.child);
    // compile-time type checking on the buf variable

    if (src.term_type(opts) != .tuple) return GetError.badarg;

    var arity: c_int = undefined;
    var src_array: [*c]const e.ErlNifTerm = undefined;

    const result = e.enif_get_tuple(options.env(opts), src.v, &arity, @ptrCast(&src_array));

    if (result == 0) return GetError.badarg;
    if (arity != child_type_info.array.len) return GetError.badarg;

    for (buf, 0..) |*slot, index| {
        slot.* = .{ .v = src_array[index] };
    }
}

pub fn get_bool(comptime T: type, src: beam.term, opts: anytype) !T {
    errdefer error_expected(T, opts);
    errdefer error_got(src, opts);

    switch (src.term_type(opts)) {
        .atom => {
            var buf: [256]u8 = undefined;
            const atom = try get_atom(src, &buf, opts);
            if (std.mem.eql(u8, "true", atom)) {
                return true;
            }
            if (std.mem.eql(u8, "false", atom)) {
                return false;
            }

            error_line(.{"note: only the atoms `true` and `false` are allowed to be bools"}, opts);
        },
        else => {},
    }

    return GetError.badarg;
}

pub fn get_array(comptime T: type, src: beam.term, opts: anytype) !T {
    errdefer error_expected(T, opts);
    errdefer error_got(src, opts);

    var result: T = undefined;
    try fill_array(T, &result, src, opts);
    return result;
}

pub fn get_pointer(comptime T: type, src: beam.term, opts: anytype) !T {
    const pointer_info = @typeInfo(T).pointer;
    switch (pointer_info.size) {
        .one => {
            return get_onepointer(T, src, opts);
        },
        .slice => {
            return get_slice(T, src, opts);
        },
        .many => {
            return get_manypointer(T, src, opts);
        },
        .c => {
            return get_cpointer(T, src, opts);
        },
    }
}

pub fn get_optional(comptime T: type, src: beam.term, opts: anytype) !T {
    errdefer error_expected(T, opts);
    errdefer error_got(src, opts);

    const Child = @typeInfo(T).optional.child;
    switch (src.term_type(opts)) {
        .atom => return try null_or_atom(T, src, opts),
        else => return try get(Child, src, opts),
    }
}

pub fn get_onepointer(comptime T: type, src: beam.term, opts: anytype) !T {
    const Child = @typeInfo(T).pointer.child;
    if (@hasField(@TypeOf(opts), "in_out")) {
        // space was provided on the stack.
        opts.in_out.* = try get(Child, src, opts);
        return opts.in_out;
    } else {
        // we must allocate space.
        const alloc = options.allocator(opts);
        const result = try alloc.create(Child);
        errdefer alloc.destroy(result);
        try fill(Child, result, src, opts);
        return result;
    }
}

pub fn get_slice(comptime T: type, src: beam.term, opts: anytype) !T {
    errdefer error_expected(T, opts);
    errdefer error_got(src, opts);

    switch (src.term_type(opts)) {
        .bitstring => return get_slice_binary(T, src, opts),
        .list => return get_slice_list(T, src, opts),
        else => return GetError.badarg,
    }
}

pub fn get_slice_binary(comptime T: type, src: beam.term, opts: anytype) !T {
    const slice_info = @typeInfo(T).pointer;
    const Child = slice_info.child;

    const byte_size_condition = get_byte_size(T) orelse return GetError.badarg;
    const expected_size_multiple = byte_size_condition.variable;

    var str_res: e.ErlNifBinary = undefined;
    if (e.enif_inspect_binary(options.env(opts), src.v, &str_res) == 0) return GetError.unreachable_error;

    if (str_res.size % expected_size_multiple != 0) {
        error_line(.{ "got: ", .{ .inspect, str_res.size } }, opts);
        error_line(.{ "note: binary size must be a multiple of ", .{ .inspect, expected_size_multiple } }, opts);
        return GetError.badarg;
    }

    const item_count = str_res.size / expected_size_multiple;
    const result_ptr = @as([*]Child, @ptrCast(@alignCast(str_res.data)));

    if (slice_info.is_const) {
        return result_ptr[0..item_count];
    } else {
        const sentinel_ptr: ?*const Child = if (slice_info.sentinel_ptr) |sentinel| @ptrCast(@alignCast(sentinel)) else options.sentinel_ptr(Child, opts);

        const alloc = options.allocator(opts);
        const alloc_count = if (sentinel_ptr) |_| item_count + 1 else item_count;

        const result = alloc.alloc(Child, alloc_count) catch |err| {
            return err;
        };

        if (sentinel_ptr) |sentinel| {
            @memcpy(result[0..item_count], result_ptr[0..item_count]);
            result[alloc_count - 1] = sentinel.*;
        } else {
            @memcpy(result, result_ptr[0..item_count]);
        }

        return @as(T, @ptrCast(result));
    }
}

pub fn get_slice_list(comptime T: type, src: beam.term, opts: anytype) !T {
    const slice_info = @typeInfo(T).pointer;
    const Child = slice_info.child;
    var length: c_uint = undefined;
    const alloc = options.allocator(opts);

    const sentinel_ptr: ?*const Child = if (slice_info.sentinel_ptr) |sentinel| @ptrCast(@alignCast(sentinel)) else options.sentinel_ptr(Child, opts);

    if (e.enif_get_list_length(options.env(opts), src.v, &length) == 0) return GetError.unreachable_error;
    const alloc_length = if (sentinel_ptr) |_| length + 1 else length;

    const result = try alloc.alloc(Child, alloc_length);
    errdefer alloc.free(result);

    var list: e.ErlNifTerm = src.v;
    for (result, 0..) |*item, index| {
        var head: e.ErlNifTerm = undefined;
        if (e.enif_get_list_cell(options.env(opts), list, &head, &list) == 0) return GetError.unreachable_error;
        item.* = get(Child, .{ .v = head }, opts) catch |err| {
            if (err == GetError.badarg) {
                error_enter(.{ "at index ", .{ .inspect, index }, ":" }, opts);
            }
            return err;
        };
    }

    if (e.enif_is_empty_list(options.env(opts), list) == 0) return GetError.unreachable_error;

    if (sentinel_ptr) |sentinel| {
        result[length] = sentinel.*;
    }

    return @as(T, @ptrCast(result));
}

pub fn get_manypointer(comptime T: type, src: beam.term, opts: anytype) !T {
    // this is equivalent to creating a slice and then discarding the length term
    const Child = @typeInfo(T).pointer.child;
    const slice = try get_slice([]Child, src, opts);
    const result = @as(T, @ptrCast(slice.ptr));
    if (@typeInfo(T).pointer.sentinel_ptr) |sentinel| {
        result[slice.len] = @as(*const Child, @ptrCast(@alignCast(sentinel))).*;
    }
    if (@hasField(@TypeOf(opts), "size")) {
        opts.size.* = slice.len;
    }
    if (@hasField(@TypeOf(opts), "in_out")) {
        opts.in_out.* = result.ptr;
    }
    return result;
}

pub fn get_cpointer(comptime T: type, src: beam.term, opts: anytype) !T {
    errdefer error_expected(T, opts);
    errdefer error_got(src, opts);

    const Child = @typeInfo(T).pointer.child;
    // scan on the type of the source.
    switch (src.term_type(opts)) {
        .atom => return try null_or_atom(T, src, opts),
        .map => if (@typeInfo(Child) != .@"struct") {
            return GetError.badarg;
        } else {
            // we have to allocate this as a slice, so that it can be safely cleaned later.
            const alloc = options.allocator(opts);
            var result = try alloc.alloc(Child, 1);
            errdefer alloc.free(result);

            try fill(Child, &result[0], src, opts);

            if (@hasField(@TypeOf(opts), "size")) {
                opts.size.* = 1;
            }

            if (@hasField(@TypeOf(opts), "in_out")) {
                opts.in_out.* = result.ptr;
            }

            return result.ptr;
        },
        .list => {
            const result_slice = try get_slice_list([]Child, src, opts);
            if (@hasField(@TypeOf(opts), "size")) {
                opts.size.* = result_slice.len;
            }

            if (@hasField(@TypeOf(opts), "in_out")) {
                opts.in_out.* = result_slice.ptr;
            }

            return result_slice.ptr;
        },
        .bitstring => {
            const result_slice = try get_slice_binary([]Child, src, opts);

            if (@hasField(@TypeOf(opts), "size")) {
                opts.size.* = result_slice.len;
            }

            if (@hasField(@TypeOf(opts), "in_out")) {
                opts.in_out.* = result_slice.ptr;
            }

            return result_slice.ptr;
        },
        else => return GetError.badarg,
    }
}

// fill functions
fn fill(comptime T: type, result: *T, src: beam.term, opts: anytype) GetError!void {
    switch (@typeInfo(T)) {
        .array => try fill_array(T, result, src, opts),
        .@"struct" => try fill_struct(T, result, src, opts),
        else => {
            @compileLog(T);
            @compileError("unhandlable type encountered in fill");
        },
    }
}

fn fill_array(comptime T: type, result: *T, src: beam.term, opts: anytype) GetError!void {
    const array_info = @typeInfo(T).array;
    const Child = array_info.child;
    switch (src.term_type(opts)) {
        .list => {
            // try to fill the array, if the lengths mismatch, then throw an error.
            // however, don't call enif_get_list_length because that incurs a second
            // pass through the array.
            var tail = src.v;
            for (result, 0..) |*item, index| {
                var head: e.ErlNifTerm = undefined;
                if (e.enif_get_list_cell(options.env(opts), tail, &head, &tail) != 0) {
                    item.* = get(Child, .{ .v = head }, opts) catch |err| {
                        if (err == GetError.badarg) {
                            error_enter(.{ "at index ", .{ .inspect, index }, ":" }, opts);
                        }
                        return err;
                    };
                } else {
                    error_line(.{ "note: length ", .{ .inspect, array_info.len }, " expected but got length ", .{ .inspect, index } }, opts);
                    return GetError.badarg;
                }
            }
            if (e.enif_is_empty_list(options.env(opts), tail) == 0) {
                var list_len: c_uint = undefined;
                if (e.enif_get_list_length(options.env(opts), tail, &list_len) == 0) return GetError.unreachable_error;
                error_line(.{ "note: length ", .{ .inspect, array_info.len }, " expected but got length ", .{ .inspect, list_len + array_info.len } }, opts);
                return GetError.badarg;
            }
        },
        .bitstring => {
            beam.ignore_when_sema();

            const byte_size_condition = get_byte_size(T) orelse return GetError.badarg;
            const expected_size = byte_size_condition.fixed;

            var str_res: e.ErlNifBinary = undefined;
            var u8_result_ptr = @as([*]u8, @ptrCast(result));

            if (e.enif_inspect_binary(options.env(opts), src.v, &str_res) == 0) return GetError.unreachable_error;

            if (str_res.size != expected_size) {
                error_line(.{ "note: binary size ", .{ .inspect, expected_size }, " expected but got size ", .{ .inspect, str_res.size } }, opts);
                return GetError.badarg;
            }

            @memcpy(u8_result_ptr[0..str_res.size], str_res.data[0..str_res.size]);
        },
        else => return GetError.badarg,
    }
}

fn fill_struct(comptime T: type, result: *T, src: beam.term, opts: anytype) !void {
    const struct_info = @typeInfo(T).@"struct";
    switch (src.term_type(opts)) {
        .map => {
            var failed: bool = false;
            // look for each of the fields:
            inline for (struct_info.fields) |field| {
                const F = field.type;
                const field_atom = beam.make_into_atom(field.name, .{ .env = options.env(opts) });
                var map_value: e.ErlNifTerm = undefined;
                if (e.enif_get_map_value(options.env(opts), src.v, field_atom.v, &map_value) == 1) {
                    @field(result.*, field.name) = get(F, .{ .v = map_value }, opts) catch |err| {
                        if (err == GetError.badarg) {
                            error_enter(.{ "in field `:", field.name, "`:" }, opts);
                        }
                        return err;
                    };
                } else {
                    // note that this is a comptime if.
                    if (field.default_value_ptr) |default_value| {
                        @field(result.*, field.name) = @as(*const F, @ptrCast(@alignCast(default_value))).*;
                    } else {
                        // can't return this directly due to compilation error.
                        failed = true;
                    }
                }

                if (failed) {
                    error_line(.{ "note: ", .{ .typename, @typeName(T) }, " requires the field `:", field.name, "`, which is missing.)" }, opts);
                    return GetError.badarg;
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

            while (e.enif_get_list_cell(options.env(opts), list, &head, &tail) == 1) : (list = tail) {
                const item: beam.term = .{ .v = head };
                try get_tuple_to_buf(item, &tuple_buf, opts);
                const key = tuple_buf[0];
                const value = tuple_buf[1];
                const atom_name = try get_atom(key, &atom_buf, opts);

                // scan the list of fields to see if we have found one.
                scan_fields: inline for (struct_info.fields) |field| {
                    if (std.mem.eql(u8, atom_name, field.name)) {
                        @field(result.*, field.name) = get(field.type, value, opts) catch |err| {
                            if (err == GetError.badarg) {
                                error_enter(.{ "in field `:", field.name, "`:" }, opts);
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
                    const Tf = field.type;
                    if (field.default_value_ptr) |defaultptr| {
                        @field(result.*, field.name) = @as(*const Tf, @ptrCast(@alignCast(defaultptr))).*;
                    } else {
                        error_line(.{ "note: ", .{ .typename, @typeName(T) }, " requires the field `:", field.name, "`, which is missing.)" }, opts);
                        return GetError.badarg;
                    }
                }
            }
        },
        .bitstring => {
            switch (struct_info.layout) {
                .@"packed", .@"extern" => {
                    const B = [@sizeOf(T)]u8;
                    const bits = @as(*align(@alignOf(T)) B, @ptrCast(result));
                    try fill_array(B, bits, src, opts);
                },
                else => return GetError.badarg,
            }
        },
        else => return GetError.badarg,
    }
}

pub fn StructRegistry(comptime SourceStruct: type) type {
    const source_info = @typeInfo(SourceStruct);
    if (source_info != .@"struct") @compileError("StructRegistry may only be called with a struct type");
    const source_fields = source_info.@"struct".fields;
    const default = false;

    var fields: [source_fields.len]std.builtin.Type.StructField = undefined;

    for (source_fields, 0..) |source_field, index| {
        fields[index] = .{ .name = source_field.name, .type = bool, .default_value_ptr = &default, .is_comptime = false, .alignment = @alignOf(*bool) };
    }

    const decls = [0]std.builtin.Type.Declaration{};
    const constructed_struct = std.builtin.Type.Struct{
        .layout = .auto,
        .fields = fields[0..],
        .decls = decls[0..],
        .is_tuple = false,
    };

    return @Type(.{ .@"struct" = constructed_struct });
}

fn null_or_atom(comptime T: type, src: beam.term, opts: anytype) !T {
    var buf: [256]u8 = undefined;
    const atom = get_atom(src, &buf, opts) catch unreachable;
    if (std.mem.eql(u8, "nil", atom)) return null;

    switch (@typeInfo(T)) {
        .optional => |o| switch (@typeInfo(o.child)) {
            .@"enum" => return try get_enum(o.child, src, opts),
            .bool => return try get_bool(o.child, src, opts),
            else => {},
        },
        else => {},
    }

    error_line(.{ "note: ", .{ .typename, @typeName(T) }, " can take the atom `nil` but no other atom" }, opts);
    return GetError.badarg;
}

inline fn error_line(msg: anytype, opts: anytype) void {
    // note that this function completely no-ops if the opts variable (which
    // in many cases is going to be `.{}`).  For the most part, this should be
    // used when detailed errors resulting from retrieving values from the VM
    // needs to be passed back to the VM.

    // in order to pass values back, .{} should contain the `.error_info` field
    // and this field should be a pointer to a `beam.term` object.  Anything
    // else will result in a compiler error.

    if (!@hasField(@TypeOf(opts), "error_info")) return;

    inline for (@typeInfo(@TypeOf(opts)).@"struct".fields) |field| {
        if (std.mem.eql(u8, "error_info", field.name)) {
            const field_type = @TypeOf(opts.error_info);
            if (field_type != *beam.term) {
                const error_msg = comptime mblk: {
                    break :mblk std.fmt.comptimePrint("the `.error_info` field of the get opts parameter must be `*beam.term`, got: {}", .{field_type});
                };
                @compileError(error_msg);
            }

            opts.error_info.v = e.enif_make_list_cell(options.env(opts), beam.make(msg, opts).v, opts.error_info.v);
        }
    }
}

inline fn error_expected(comptime T: type, opts: anytype) void {
    // it's not entirely obvious why, but this needs to be put into a comptime block
    // to avoid a memory aliasing bug that exists in ziglang. (0.10.0)
    const typespec = comptime ts: {
        break :ts typespec_for(T);
    };

    const typename = comptime tn: {
        break :tn typename_for(T);
    };

    error_line(.{ "expected: ", typespec, " (for `", .{ .typename, typename }, "`)" }, opts);
}

inline fn error_got(src: beam.term, opts: anytype) void {
    error_line(.{ "got: `", .{ .inspect, src }, "`" }, opts);
}

inline fn error_enter(msg: anytype, opts: anytype) void {
    error_line(.enter, opts);
    error_line(msg, opts);
}

fn typespec_for(comptime T: type) []const u8 {
    return switch (@typeInfo(T)) {
        .int => "integer",
        .@"enum" => |en| make_enum: {
            comptime {
                var typespec: []const u8 = "";
                var should_pipe = false;

                for (en.fields) |field| {
                    if (should_pipe) {
                        typespec = typespec ++ " | ";
                    }
                    typespec = typespec ++ std.fmt.comptimePrint("{}", .{field.value});
                    should_pipe = true;
                }

                for (en.fields) |field| {
                    typespec = typespec ++ " | " ++ ":" ++ field.name[0..];
                }

                break :make_enum typespec;
            }
        },
        .float => "float | :infinity | :neg_infinity | :NaN",
        // a struct might be a reosurce.  If it's not, then it could be map/keyword, or maybe a binary (if packed or extern).
        .@"struct" => |s| if (resource.MaybeUnwrap(s)) |_| "reference" else "map | keyword" ++ binary_spec(T),
        .bool => "boolean",
        .array => |a| "list(" ++ typespec_for(a.child) ++ ")" ++ binary_spec(T),
        .pointer => |p| switch (p.size) {
            // pointer to one can only be a map or keyword.
            .one => "map | keyword",
            .slice => "list(" ++ typespec_for(p.child) ++ ")" ++ binary_spec(T),
            .many => "list(" ++ typespec_for(p.child) ++ ")" ++ binary_spec(T),
            .c => (if (@typeInfo(p.child) == .@"struct") "map | list(" ++ typespec_for(p.child) ++ ")" else "") ++ binary_spec(T),
        },
        .optional => |o| comptime make_optional: {
            break :make_optional "nil | " ++ typespec_for(o.child);
        },
        else => @compileError("unreachable"),
    };
}

fn typename_for(comptime T: type) []const u8 {
    return switch (@typeInfo(T)) {
        .@"struct" => |s| if (resource.MaybeUnwrap(s)) |_| refname_for(T) else @typeName(T),
        else => @typeName(T),
    };
}

fn refname_for(comptime T: type) []const u8 {
    inline for (@typeInfo(T).@"struct".fields) |field| {
        if (std.mem.eql(u8, field.name, "__payload")) {
            return "beam.Resource(" ++ @typeName(@typeInfo(field.type).pointer.child) ++ ", @import(\"root\"), .{...})";
        }
    }
    unreachable;
}

const BinarySizeTag = enum { fixed, variable };
const BinarySize = union(BinarySizeTag) {
    fixed: usize,
    variable: usize,
};

fn binary_spec(comptime T: type) []const u8 {
    if (get_byte_size(T)) |size| {
        return switch (size) {
            .fixed => |s| std.fmt.comptimePrint(" | <<_::{}>>", .{s * 8}),
            .variable => |s| std.fmt.comptimePrint(" | <<_::_*{}>>", .{s * 8}),
        };
    }
    return "";
}

fn get_byte_size(comptime T: type) ?BinarySize {
    // function is null if the type can't be a binary, otherwise it returns a variable or
    // fixed byte size specification
    const info = @typeInfo(T);
    switch (info) {
        .array => |a| {
            // arrays can only be binary if the associated child size is integer, float, or fixed, and the
            // size is fixed.
            switch (@typeInfo(a.child)) {
                .int, .float => return .{ .fixed = @sizeOf(a.child) * a.len },
                else => {},
            }
            if (get_byte_size(a.child)) |bsc| {
                if (bsc == .fixed) {
                    return .{ .fixed = bsc.fixed * a.len };
                }
            }
            return null;
        },
        .pointer => |p| {
            switch (p.size) {
                .slice, .many, .c => {
                    // pointers can only be binary if the associated child size is integer, float, or fixed, but
                    // the result is variable.
                    switch (@typeInfo(p.child)) {
                        .int, .float => return .{ .variable = @sizeOf(p.child) },
                        else => {},
                    }
                    if (get_byte_size(p.child)) |bsc| {
                        if (bsc == .fixed) {
                            return .{ .variable = bsc.fixed };
                        }
                    }
                    return null;
                },
                .one => return null,
            }
        },
        .@"struct" => |s| {
            // structs can be binary, if the struct is either packed or extern.
            switch (s.layout) {
                .@"packed" => return .{ .fixed = @sizeOf(s.backing_integer.?) },
                .@"extern" => return .{ .fixed = @sizeOf(T) },
                .auto => return null,
            }
        },
        else => return null,
    }
}

// special getters

pub fn get_list_cell(list: beam.term, opts: anytype) !struct { beam.term, beam.term } {
    errdefer error_expected([]beam.term, opts);
    errdefer error_got(list, opts);

    const env = options.env(opts);
    var head: e.ErlNifTerm = undefined;
    var tail: e.ErlNifTerm = undefined;

    if (e.enif_get_list_cell(env, list.v, &head, &tail) == 0) return error.badarg;

    return .{ .{ .v = head }, .{ .v = tail } };
}
