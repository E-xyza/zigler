const std = @import("std");
const stubs = @import("sema_stubs.zig");
const analyte = @import("analyte");

const json = std.json;

fn streamInt(stream: anytype, comptime i: std.builtin.Type.Int) !void {
    try typeHeader(stream, "integer");
    try stream.objectField("signedness");
    switch (i.signedness) {
        .unsigned => try stream.write("unsigned"),
        .signed => try stream.write("signed"),
    }
    try stream.objectField("bits");
    try stream.write(i.bits);
}

fn streamEnum(stream: anytype, comptime en: std.builtin.Type.Enum, comptime T: type) !void {
    if (en.fields.len <= 1) {
        try typeHeader(stream, "unusable:" ++ @typeName(T));
        return;
    }

    try typeHeader(stream, "enum");
    try stream.objectField("name");
    try stream.write(@typeName(T));
    try stream.objectField("tags");
    try stream.beginObject();
    inline for (en.fields) |field| {
        try stream.objectField(field.name);
        try stream.write(field.value);
    }
    try stream.endObject();
}

fn streamFloat(stream: anytype, comptime f: std.builtin.Type.float) !void {
    try typeHeader(stream, "float");
    try stream.objectField("bits");
    try stream.write(f.bits);
}

fn streamStruct(stream: anytype, comptime s: std.builtin.Type.Struct, comptime S: type) !void {
    const name = @typeName(S);

    try typeHeader(stream, "struct");
    try stream.objectField("name");
    try stream.write(name);
    switch (s.layout) {
        .@"packed" => {
            try stream.objectField("packed_size");
            try stream.write(@bitSizeOf(S));
        },
        .@"extern" => {
            try stream.objectField("extern");
            try stream.write(true);
        },
        .auto => {},
    }
    try stream.objectField("fields");
    try stream.beginArray();
    inline for (s.fields) |field| {
        try stream.beginObject();
        try stream.objectField("name");
        try stream.write(field.name);
        try stream.objectField("type");
        try streamType(stream, field.type);
        try stream.objectField("required");
        if (field.default_value_ptr) |default_value| {
            _ = default_value;
            try stream.write(false);
        } else {
            try stream.write(true);
        }
        try stream.objectField("alignment");
        try stream.write(field.alignment);
        try stream.endObject();
    }
    try stream.endArray();
}

fn streamArray(stream: anytype, comptime a: std.builtin.Type.Array, repr: anytype) !void {
    try typeHeader(stream, "array");
    try stream.objectField("len");
    try stream.write(a.len);
    try stream.objectField("child");
    try streamType(stream, a.child);
    try stream.objectField("has_sentinel");
    try stream.write(if (a.sentinel) |_| true else false);
    try stream.objectField("repr");
    try stream.write(repr);
}

fn streamPointer(stream: anytype, comptime p: std.builtin.Type.Pointer, repr: anytype) !void {
    switch (p.size) {
        .one => {
            try typeHeader(stream, "pointer");
        },
        .many => {
            try typeHeader(stream, "manypointer");
            try stream.objectField("has_sentinel");
            try stream.write(if (p.sentinel_ptr) |_| true else false);
            try stream.objectField("repr");
            try stream.write(repr);
        },
        .slice => {
            try typeHeader(stream, "slice");
            try stream.objectField("has_sentinel");
            try stream.write(if (p.sentinel_ptr) |_| true else false);
            try stream.objectField("repr");
            try stream.write(repr);
        },
        .c => {
            try typeHeader(stream, "cpointer");
        },
    }
    try stream.objectField("is_const");
    try stream.write(p.is_const);
    try stream.objectField("child");
    try streamType(stream, p.child);
}

fn streamOptional(stream: anytype, comptime o: std.builtin.Type.Optional) !void {
    try typeHeader(stream, "optional");
    try stream.objectField("child");
    try streamType(stream, o.child);
}

fn typeHeader(stream: anytype, comptime name: []const u8) !void {
    try stream.objectField("type");
    try stream.write(name);
}

fn typematches(comptime T: type, comptime name: []const u8) bool {
    const typename = @typeName(T);
    if (typename.len < name.len) return false;
    for (name, 0..) |c, i| {
        if (typename[i] != c) return false;
    }
    return true;
}

const typemapping = .{
    .{ .match = "beam.term__struct_", .name = "term" },
    .{ .match = "stub_erl_nif.ERL_NIF_TERM", .name = "erl_nif_term" },
    .{ .match = "stub_erl_nif.ErlNifEvent", .name = "e.ErlNifEvent" },
    .{ .match = "stub_erl_nif.ErlNifBinary", .name = "e.ErlNifBinary" },
    .{ .match = "stub_erl_nif.ErlNifPid", .name = "pid" },
    .{ .match = "?*stub_erl_nif.ErlNifEnv", .name = "env" },
};

fn streamType(stream: anytype, comptime T: type) !void {
    try stream.beginObject();

    // catch any types that depend on either `beam` or `erl_nif`
    inline for (typemapping) |m| {
        if (typematches(T, m.match)) {
            try typeHeader(stream, m.name);
            try stream.endObject();
            return;
        }
    }

    switch (T) {
        std.builtin.StackTrace => {
            try typeHeader(stream, "builtin.StackTrace");
        },
        else => {
            switch (@typeInfo(T)) {
                .int => |i| try streamInt(stream, i),
                .@"enum" => |en| try streamEnum(stream, en, T),
                .float => |f| try streamFloat(stream, f),
                .@"struct" => |s| try streamStruct(stream, s, T),
                .array => |a| try streamArray(stream, a, std.fmt.comptimePrint("{}", .{T})),
                .pointer => |p| try streamPointer(stream, p, std.fmt.comptimePrint("{}", .{T})),
                .optional => |o| try streamOptional(stream, o),
                .bool => try typeHeader(stream, "bool"),
                .void => try typeHeader(stream, "void"),
                .error_union => |eu| {
                    try typeHeader(stream, "error");
                    try stream.objectField("child");
                    try streamType(stream, eu.payload);
                },
                else => {
                    try typeHeader(stream, "unusable:" ++ @typeName(T));
                },
            }
        },
    }
    try stream.endObject();
}

pub fn streamFun(stream: anytype, comptime name: anytype, comptime fun: std.builtin.Type.Fn) !void {
    try stream.beginObject();

    // emit name
    try stream.objectField("name");
    try stream.write(name);

    // emit return type
    try stream.objectField("return");
    if (fun.return_type) |return_type| {
        try streamType(stream, return_type);
    } else {
        try stream.write(null);
    }

    // emit params
    try stream.objectField("params");
    try stream.beginArray();
    inline for (fun.params) |param| {
        if (param.type) |T| {
            try streamType(stream, T);
        } else {
            try stream.write(null);
        }
    }
    try stream.endArray();
    try stream.endObject();
}

pub fn streamModule(stream: anytype, comptime Mod: type) !void {
    const mod_info = @typeInfo(Mod).@"struct";
    try stream.beginObject();
    try stream.objectField("functions");
    try stream.beginArray();
    // functions are found in decls
    inline for (mod_info.decls) |decl| {
        const decl_info = @typeInfo(@TypeOf(@field(Mod, decl.name)));
        comptime var is_stubbed: bool = false;
        inline for (stubs.functions) |stub| {
            comptime var found = std.mem.eql(u8, stub.name, decl.name);
            found = found and true;
            if (found) {
                try stub.stream(stream);
                is_stubbed = true;
            }
        }

        if (!is_stubbed and .@"fn" == decl_info) {
            try streamFun(stream, decl.name, decl_info.@"fn");
        }
    }

    try stream.endArray();

    try stream.objectField("types");
    try stream.beginArray();
    // types are found in decls
    inline for (mod_info.decls) |decl| {
        switch (@typeInfo(@TypeOf(@field(Mod, decl.name)))) {
            .type => {
                const T = @field(Mod, decl.name);
                try stream.beginObject();
                try stream.objectField("name");
                try stream.write(decl.name);
                try stream.objectField("type");
                try streamType(stream, T);
                try stream.endObject();
            },
            else => {},
        }
    }
    try stream.endArray();

    try stream.objectField("decls");
    try stream.beginArray();
    inline for (mod_info.decls) |decl| {
        switch (@typeInfo(@TypeOf(@field(Mod, decl.name)))) {
            .type => {},
            .@"fn" => {},
            else => {
                try stream.beginObject();
                try stream.objectField("name");
                try stream.write(decl.name);
                try stream.objectField("type");
                try stream.write(@typeName(@TypeOf(@field(Mod, decl.name))));
                try stream.endObject();
            },
        }
    }
    try stream.endArray();

    try stream.endObject();
}

pub fn main() !void {
    const stdout = std.fs.File.stdout();
    var buffer: [256]u8 = undefined;
    var stdout_writer = stdout.writer(&buffer);
    var stream: json.Stringify = .{ .writer = &stdout_writer.interface };

    try streamModule(&stream, analyte);
    try stdout_writer.interface.flush();
}
