const std = @import("std");
const resource = @import("resource.zig");
const beam = @import("beam.zig");
const e = @import("erl_nif.zig");

fn streamInt(stream: anytype, comptime i: std.builtin.Type.Int) !void {
    try emitType(stream, "integer");
    try stream.objectField("signedness");
    switch (i.signedness) {
        .unsigned => try stream.emitString("unsigned"),
        .signed => try stream.emitString("signed"),
    }
    try stream.objectField("bits");
    try stream.emitNumber(i.bits);
}

fn streamEnum(stream: anytype, comptime en: std.builtin.Type.Enum, comptime T: type) !void {
    if (en.fields.len <= 1) {
        try emitType(stream, "unusable:" ++ @typeName(T));
        return;
    }

    try emitType(stream, "enum");
    try stream.objectField("name");
    try stream.emitString(@typeName(T));
    try stream.objectField("tags");
    try stream.beginObject();
    inline for (en.fields) |field| {
        try stream.objectField(field.name);
        try stream.emitNumber(field.value);
    }
    try stream.endObject();
}

fn streamFloat(stream: anytype, comptime f: std.builtin.Type.Float) !void {
    try emitType(stream, "float");
    try stream.objectField("bits");
    try stream.emitNumber(f.bits);
}

fn streamStruct(stream: anytype, comptime s: std.builtin.Type.Struct, comptime name: []const u8) !void {
    if (resource.MaybeUnwrap(s)) |res_type| {
        try emitType(stream, "resource");
        try stream.objectField("name");
        try stream.emitString(name);
        try stream.objectField("payload");
        try stream.emitString(@typeName(res_type));
    } else {
        try emitType(stream, "struct");
        try stream.objectField("name");
        try stream.emitString(name);
        switch (s.layout) {
            .Packed => {
                const OriginalType = @Type(.{ .Struct = s });
                try stream.objectField("packed_size");
                try stream.emitNumber(@bitSizeOf(OriginalType));
            },
            .Extern => {
                try stream.objectField("extern");
                try stream.emitBool(true);
            },
            .Auto => {}
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
    }
}

fn streamArray(stream: anytype, comptime a: std.builtin.Type.Array, repr: anytype) !void {
    try emitType(stream, "array");
    try stream.objectField("len");
    try stream.emitNumber(a.len);
    try stream.objectField("child");
    try streamType(stream, a.child);
    try stream.objectField("has_sentinel");
    try stream.emitBool(if (a.sentinel) |_| true else false);
    try stream.objectField("repr");
    try stream.emitString(repr);
}

fn streamPointer(stream: anytype, comptime p: std.builtin.Type.Pointer, repr: anytype) !void {
    switch (p.size) {
        .One => {
            try emitType(stream, "pointer");
        },
        .Many => {
            try emitType(stream, "manypointer");
            try stream.objectField("has_sentinel");
            try stream.emitBool(if (p.sentinel) |_| true else false);
            try stream.objectField("repr");
            try stream.emitString(repr);
        },
        .Slice => {
            try emitType(stream, "slice");
            try stream.objectField("has_sentinel");
            try stream.emitBool(if (p.sentinel) |_| true else false);
            try stream.objectField("repr");
            try stream.emitString(repr);
        },
        .C => {
            try emitType(stream, "cpointer");
        },
    }
    try stream.objectField("is_const");
    try stream.emitBool(p.is_const);
    try stream.objectField("child");
    try streamType(stream, p.child);
}

fn streamOptional(stream: anytype, comptime o: std.builtin.Type.Optional) !void {
    try emitType(stream, "optional");
    try stream.objectField("child");
    try streamType(stream, o.child);
}

fn emitType(stream: anytype, comptime name: []const u8) !void {
    try stream.objectField("type");
    try stream.emitString(name);
}

var depth: usize = 0;

fn streamType(stream: anytype, comptime T: type) !void {
    try stream.beginObject();
    // catch special types pid, port and term
    switch (T) {
        e.ErlNifPid => {
            try emitType(stream, "pid");
        },
        beam.term => {
            try emitType(stream, "term");
        },
        e.ErlNifTerm => {
            try emitType(stream, "erl_nif_term");
        },
        beam.env => {
            try emitType(stream, "env");
        },
        else => {
            switch (@typeInfo(T)) {
                .Int => |i| try streamInt(stream, i),
                .Enum => |en| try streamEnum(stream, en, T),
                .Float => |f| try streamFloat(stream, f),
                .Struct => |s| try streamStruct(stream, s, @typeName(T)),
                .Array => |a| try streamArray(stream, a, std.fmt.comptimePrint("{}", .{T})),
                .Pointer => |p| try streamPointer(stream, p, std.fmt.comptimePrint("{}", .{T})),
                .Optional => |o| try streamOptional(stream, o),
                .Bool => try emitType(stream, "bool"),
                .Void => try emitType(stream, "void"),
                .ErrorUnion => |eu| {
                    try emitType(stream, "error");
                    try stream.objectField("child");
                    try streamType(stream, eu.payload);
                },
                else => {
                    try emitType(stream, "unusable:" ++ @typeName(T));
                },
            }
        },
    }
    try stream.endObject();
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
                else => {},
            }
        }
    }
    try stream.endArray();
    try stream.endObject();
}
