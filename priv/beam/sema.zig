const std = @import("std");

fn streamInt(stream: anytype, comptime i: std.builtin.Type.Int) !void {
    try stream.beginObject();
    try stream.objectField("type");
    try stream.emitString("integer");
    try stream.objectField("signedness");
    switch (i.signedness) {
      .unsigned =>
        try stream.emitString("unsigned"),
      .signed =>
        try stream.emitString("signed"),
    }
    try stream.objectField("bits");
    try stream.emitNumber(i.bits);
    try stream.endObject();
}

fn streamEnum(stream: anytype, comptime e: std.builtin.Type.Enum, comptime T: type) !void {
    try stream.beginObject();
    try stream.objectField("type");
    try stream.emitString("enum");
    try stream.objectField("name");
    try stream.emitString(@typeName(T));
    try stream.objectField("tags");
    try stream.beginObject();
    inline for (e.fields) |field| {
        try stream.objectField(field.name);
        try stream.emitNumber(field.value);
    }
    try stream.endObject();
    try stream.endObject();
}

fn streamFloat(stream: anytype, comptime f: std.builtin.Type.Float) !void {
    try stream.beginObject();
    try stream.objectField("type");
    try stream.emitString("float");
    try stream.objectField("bits");
    try stream.emitNumber(f.bits);
    try stream.endObject();
}

fn streamStruct(stream: anytype, comptime s: std.builtin.Type.Struct, comptime name: []const u8) !void {
    try stream.beginObject();
    try stream.objectField("name");
    try stream.emitString(name);
    try stream.objectField("type");
    try stream.emitString("struct");
    if (s.layout == .Packed) {
        const OriginalType = @Type(.{.Struct = s});
        try stream.objectField("packed_size");
        try stream.emitNumber(@bitSizeOf(OriginalType));
    }
    try stream.objectField("fields");
    try stream.beginArray();
    inline for (s.fields) |field| {
        try stream.arrayElem();
        try stream.beginObject();
        try stream.objectField("name");
        try stream.emitString(field.name);
        try stream.objectField("type");
        try streamType(stream, field.field_type);
        try stream.objectField("required");
        if (field.default_value) |default_value| {
            _ = default_value;
            try stream.emitBool(false);
        } else {
            try stream.emitBool(true);
        }
        try stream.objectField("alignment");
        try stream.emitNumber(field.alignment);
        try stream.endObject();
    }
    try stream.endArray();
    try stream.endObject();
}

fn streamArray(stream: anytype, comptime a: std.builtin.Type.Array, repr: anytype) !void {
    try stream.beginObject();
    try stream.objectField("type");
    try stream.emitString("array");
    try stream.objectField("len");
    try stream.emitNumber(a.len);
    try stream.objectField("child");
    try streamType(stream, a.child);
    try stream.objectField("hasSentinel");
    try stream.emitBool(if (a.sentinel) |_| true else false);
    try stream.objectField("repr");
    try stream.emitString(repr);
    // TODO: figure out sentinels
    try stream.endObject();
}

fn streamPointer(stream: anytype, comptime p: std.builtin.Type.Pointer, repr: anytype) !void {
    switch (p.size) {
        .One => {
            try stream.beginObject();
            try stream.objectField("type");
            try stream.emitString("pointer");
            try stream.objectField("child");
            try streamType(stream, p.child);
            try stream.endObject();
        },
        .Many => {
            @compileError("not implemented yet");
        },
        .Slice => {
            try stream.beginObject();
            try stream.objectField("type");
            try stream.emitString("slice");
            try stream.objectField("child");
            try streamType(stream, p.child);
            try stream.objectField("hasSentinel");
            try stream.emitBool(if (p.sentinel) |_| true else false);
            try stream.objectField("repr");
            try stream.emitString(repr);
            try stream.endObject();
        },
        .C => {
            @compileError("not implemented yet");
        }
    }
}

fn streamOptional(stream: anytype, comptime o: std.builtin.Type.Optional) ! void {
    try stream.beginObject();
    try stream.objectField("type");
    try stream.emitString("optional");
    try stream.objectField("child");
    try streamType(stream, o.child);
    try stream.endObject();
}

fn streamType(stream: anytype, comptime T: type) !void {
    switch (@typeInfo(T)) {
        .Int => |i|
            try streamInt(stream, i),
        .Enum => |e|
            try streamEnum(stream, e, T),
        .Float => |f|
            try streamFloat(stream, f),
        .Struct => |s|
            try streamStruct(stream, s, @typeName(T)),
        .Array => |a|
            try streamArray(stream, a, std.fmt.comptimePrint("{}", .{T})),
        .Pointer => |p|
            try streamPointer(stream, p, std.fmt.comptimePrint("{}", .{T})),
        .Optional => |o|
            try streamOptional(stream, o),
        else =>
            try stream.emitString(std.fmt.comptimePrint("{}", .{T})),
    }
}

pub fn streamFun(stream: anytype, comptime name: anytype, comptime fun: std.builtin.Type.Fn) !void {
    try stream.beginObject();
    try stream.objectField("name");
    try stream.emitString(name);
    try stream.objectField("return");
    try streamType(stream, fun.return_type.?);
    try stream.objectField("params");
    try stream.beginArray();
    inline for (fun.args) |arg| {
        try stream.arrayElem();
        try streamType(stream, arg.arg_type.?);
    }
    try stream.endArray();
    try stream.endObject();
}

pub fn streamModule(stream: anytype, comptime Mod: type) !void {
    const mod_info = @typeInfo(Mod).Struct;
    try stream.beginObject();
    try stream.objectField("functions");
    try stream.beginArray();
    inline for (mod_info.decls) |decl| {
        if (decl.is_pub) {
            switch (@typeInfo(@TypeOf(@field(Mod, decl.name)))) {
                .Fn => |fun| {
                    try stream.arrayElem();
                    try streamFun(stream, decl.name, fun);
                },
                // do something about types.
                else => {}
            }
        }
    }
    try stream.endArray();
    try stream.endObject();
}