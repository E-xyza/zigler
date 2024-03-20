const std = @import("std");
const e = @import("erl_nif");
const beam = @import("beam.zig");

pub fn Payload(comptime function: anytype) type {
    const T = if (@TypeOf(function) == type) function else @TypeOf(function);

    const params = switch (@typeInfo(T)) {
        .Fn => |f| f.params,
        else => @compileError("Payload is only available for a function"),
    };

    const SF = std.builtin.Type.StructField;

    var fields: []const SF = &[_]SF{};
    const decls = [0]std.builtin.Type.Declaration{};

    for (params, 0..) |param, index| {
        const new_field = [1]SF{.{
            .name = std.fmt.comptimePrint("{}", .{index}),
            .type = param.type.?,
            .default_value = null,
            .is_comptime = false,
            .alignment = 4,
        }};
        fields = fields ++ &new_field;
    }

    const result_type_info: std.builtin.Type = .{ .Struct = .{
        .layout = .Auto,
        .fields = fields,
        .decls = &decls,
        .is_tuple = true,
    } };

    return @Type(result_type_info);
}

// gets the arity of a function f

fn arity(fun: anytype) u8 {
    return switch (@typeInfo(@TypeOf(fun))) {
        .Fn => |f| f.params.len,
        .Struct => |s| s.fields.len,
        else => @compileError("arity is only available for a function"),
    };
}

// builds up a payload tuple for a function.  the error_index parameter is an in-out parameter that stores the
// index of which parameter failed to be stuffed into the payload tuple.  Only access it in the case that
// build function fails.
pub fn build(fun: anytype, argc: c_int, args: [*c]const e.ErlNifTerm, error_index: *u8, build_opts: anytype) !Payload(fun) {
    // assertions
    const arity_ = comptime arity(fun);
    if (argc != arity_) {
        @panic("nif function called with wrong arity");
    }
    if (build_opts.len != arity_) {
        @compileError("mismatched arity and build opts");
    }

    var result: Payload(fun) = undefined;
    comptime var index = 0;
    inline while (index < arity_) : (index += 1) {
        error_index.* = index;
        result[index] = try beam.get(@TypeOf(result[index]), .{ .v = args[index] }, build_opts[index]);
    }
    return result;
}

pub fn cleanup(payload: anytype, should_clean: anytype) void {
    if (@TypeOf(should_clean) == @TypeOf(null)) return;

    inline for (should_clean, 0..) |item_clean_opts, index| {
        if (@TypeOf(item_clean_opts) == @TypeOf(null)) continue;
        beam.cleanup(payload[index], item_clean_opts);
    }
}
