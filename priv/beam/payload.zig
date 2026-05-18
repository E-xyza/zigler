const std = @import("std");
const e = @import("erl_nif");
const beam = @import("beam.zig");

pub fn Payload(comptime function: anytype) type {
    const T = if (@TypeOf(function) == type) function else @TypeOf(function);

    const params = switch (@typeInfo(T)) {
        .@"fn" => |f| f.params,
        else => @compileError("Payload is only available for a function"),
    };

    var types: []const type = &[_]type{};

    for (params) |param| {
        types = types ++ &[_]type{param.type.?};
    }

    return @Tuple(types);
}

// gets the arity of a function f

fn arity(fun: anytype) u8 {
    return switch (@typeInfo(@TypeOf(fun))) {
        .@"fn" => |f| f.params.len,
        .@"struct" => |s| s.fields.len,
        else => @compileError("arity is only available for a function"),
    };
}

// builds up a payload tuple for a function.  the error_index parameter is an in-out parameter that stores the
// index of which parameter failed to be stuffed into the payload tuple.  Only access it in the case that
// build function fails.
pub fn build(fun: anytype, argc: c_int, args: [*c]const e.ErlNifTerm, error_index: *u8, payload_opts: anytype) !Payload(fun) {
    // assertions
    const arity_ = comptime arity(fun);
    if (argc != arity_) {
        @panic("nif function called with wrong arity");
    }
    if (payload_opts.len != arity_) {
        @compileError("mismatched arity and build opts");
    }

    var result: Payload(fun) = undefined;
    comptime var index = 0;
    inline while (index < arity_) : (index += 1) {
        error_index.* = index;
        result[index] = try beam.get(@TypeOf(result[index]), .{ .v = args[index] }, payload_opts[index]);
    }
    return result;
}

pub fn cleanup(payload: anytype, opts_list: anytype) void {
    inline for (opts_list, 0..) |opts, index| {
        beam.cleanup(payload[index], opts);
    }
}
