const std = @import("std");
const e = @import("erl_nif");
const beam = @import("beam.zig");

pub fn Payload(comptime function: anytype) type {
    const T = if (@TypeOf(function) == type) function else @TypeOf(function);

    const args = switch (@typeInfo(T)) {
        .Fn => |f| f.args,
        else => @compileError("Payload is only available for a function"),
    };

    const SF = std.builtin.Type.StructField;

    var fields: []const SF = &[_]SF{};
    const decls = [0]std.builtin.Type.Declaration{};

    for (args, 0..) |arg, index| {
        const new_field = [1]SF{.{
            .name = std.fmt.comptimePrint("{}", .{index}),
            .field_type = arg.arg_type.?,
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

// checks to see if the function has env as its first term

fn is_env_first(fun: anytype) bool {
    const T: type = if (@TypeOf(fun) == type) fun else @TypeOf(fun);

    return switch (@typeInfo(T)) {
        .Fn => |f| (f.args.len > 0) and (f.args[0].arg_type == beam.env),
        .Struct => |s| (s.fields.len > 0) and (s.fields[0].field_type == beam.env),
        else => @compileError("is_env_first is only available for a functions and Payloads"),
    };
}

// gets the arity of a function f

fn arity(fun: anytype) u8 {
    return switch (@typeInfo(@TypeOf(fun))) {
        .Fn => |f| if (comptime is_env_first(fun)) f.args.len - 1 else f.args.len,
        .Struct => |s| if (comptime is_env_first(fun)) s.fields.len - 1 else s.fields.len,
        else => @compileError("arity is only available for a function"),
    };
}

// builds up a payload tuple for a function.  the error_index parameter is an in-out parameter that stores the
// index of which parameter failed to be stuffed into the payload tuple.  Only access it in the case that
// build function fails.
pub fn build(fun: anytype, env: beam.env, argc: c_int, args: [*c]const e.ErlNifTerm, error_index: *u8, build_opts: anytype) !Payload(fun) {
    if (comptime is_env_first(fun)) {
        return build_with_env(fun, env, argc, args, error_index, build_opts);
    } else {
        return build_no_env(fun, env, argc, args, error_index, build_opts);
    }
}

fn build_with_env(fun: anytype, env: beam.env, argc: c_int, args: [*c]const e.ErlNifTerm, error_index: *u8, build_opts: anytype) !Payload(fun) {
    // assertions
    const arity_ = comptime arity(fun);
    if (argc != arity_) {
        @panic("nif function called with wrong arity");
    }

    var payload: Payload(fun) = undefined;

    payload[0] = env;

    comptime var arg_index = 0;
    inline while (arg_index < arity_) : (arg_index += 1) {
        const payload_index = arg_index + 1;
        error_index.* = arg_index;
        payload[payload_index] = try beam.get(@TypeOf(payload[payload_index]), env, .{ .v = args[arg_index] }, build_opts[arg_index]);
    }
    return payload;
}

fn build_no_env(fun: anytype, env: beam.env, argc: c_int, args: [*c]const e.ErlNifTerm, error_index: *u8, build_opts: anytype) !Payload(fun) {
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
        result[index] = try beam.get(@TypeOf(result[index]), env, .{ .v = args[index] }, build_opts[index]);
    }
    return result;
}

pub fn cleanup(payload: anytype, should_clean: anytype) void {
    if (@TypeOf(should_clean) == @TypeOf(null)) return;

    if (comptime is_env_first(@TypeOf(payload))) {
        cleanup_with_env(payload, should_clean);
    } else {
        cleanup_no_env(payload, should_clean);
    }
}

fn cleanup_with_env(payload: anytype, should_clean: anytype) void {
    inline for (should_clean, 0..) |item_clean_opts, index| {
        if (@TypeOf(item_clean_opts) == @TypeOf(null)) continue;
        beam.cleanup(payload[index + 1], item_clean_opts);
    }
}

fn cleanup_no_env(payload: anytype, should_clean: anytype) void {
    inline for (should_clean, 0..) |item_clean_opts, index| {
        if (@TypeOf(item_clean_opts) == @TypeOf(null)) continue;
        beam.cleanup(payload[index], item_clean_opts);
    }
}
