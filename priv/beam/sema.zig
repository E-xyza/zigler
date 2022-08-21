const std = @import("std");

fn streamInt(stream: anytype, comptime i: anytype) !void {
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

fn streamType(stream: anytype, comptime T: type) !void {
    switch (@typeInfo(T)) {
        .Int => |i|
            try streamInt(stream, i),
        else =>
            try stream.emitString(std.fmt.comptimePrint("{}", .{T})),
    }
}

pub fn streamFun(stream: anytype, comptime name: anytype, fun: std.builtin.Type.Fn) !void {
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
    try stream.beginArray();
    inline for (mod_info.decls) |decl| {
        if (decl.is_pub) {
            try stream.arrayElem();
            try streamFun(stream, decl.name, @typeInfo(@TypeOf(@field(Mod, decl.name))).Fn);
        }
    }
    try stream.endArray();
}