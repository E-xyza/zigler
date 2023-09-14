const std = @import("std");
const beam = @import("beam");
const e = @import("erl_nif");

const analyte = @import("analyte");
const json = std.json;

// possibly 200 is an over-conservative choice for function depth here.
const WriteError = std.fs.File.WriteError;
const FileWriter = std.io.Writer(std.fs.File, WriteError, std.fs.File.write);
const JsonStreamPtr = *json.WriteStream(FileWriter, .{ .assumed_correct = {} });

fn streamInt(stream: anytype, comptime i: std.builtin.Type.Int) WriteError!void {
    try beginType(stream, "integer");
    try stream.objectField("signedness");
    switch (i.signedness) {
        .unsigned => try stream.write("unsigned"),
        .signed => try stream.write("signed"),
    }
    try stream.objectField("bits");
    try stream.write(i.bits);
}

fn streamEnum(stream: anytype, comptime en: std.builtin.Type.Enum, comptime T: type) WriteError!void {
    if (en.fields.len <= 1) {
        try beginType(stream, "unusable:" ++ @typeName(T));
        return;
    }

    try beginType(stream, "enum");
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

fn streamFloat(stream: anytype, comptime f: std.builtin.Type.Float) WriteError!void {
    try beginType(stream, "float");
    try stream.objectField("bits");
    try stream.write(f.bits);
}

fn streamStruct(stream: anytype, comptime s: std.builtin.Type.Struct, comptime S: type) WriteError!void {
    const name = @typeName(S);

    try beginType(stream, "struct");
    try stream.objectField("name");
    try stream.write(name);
    switch (s.layout) {
        .Packed => {
            try stream.objectField("packed_size");
            try stream.write(@bitSizeOf(S));
        },
        .Extern => {
            try stream.objectField("extern");
            try stream.write(true);
        },
        .Auto => {},
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
        if (field.default_value) |default_value| {
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
    //}
}

fn streamArray(stream: anytype, comptime a: std.builtin.Type.Array, repr: anytype) WriteError!void {
    try beginType(stream, "array");
    try stream.objectField("len");
    try stream.write(a.len);
    try stream.objectField("child");
    try streamType(stream, a.child);
    try stream.objectField("has_sentinel");
    try stream.write(if (a.sentinel) |_| true else false);
    try stream.objectField("repr");
    try stream.write(repr);
}

fn streamPointer(stream: anytype, comptime p: std.builtin.Type.Pointer, repr: anytype) WriteError!void {
    switch (p.size) {
        .One => {
            try beginType(stream, "pointer");
        },
        .Many => {
            try beginType(stream, "manypointer");
            try stream.objectField("has_sentinel");
            try stream.write(if (p.sentinel) |_| true else false);
            try stream.objectField("repr");
            try stream.write(repr);
        },
        .Slice => {
            try beginType(stream, "slice");
            try stream.objectField("has_sentinel");
            try stream.write(if (p.sentinel) |_| true else false);
            try stream.objectField("repr");
            try stream.write(repr);
        },
        .C => {
            try beginType(stream, "cpointer");
        },
    }
    try stream.objectField("is_const");
    try stream.write(p.is_const);
    try stream.objectField("child");
    try streamType(stream, p.child);
}

fn streamOptional(stream: anytype, comptime o: std.builtin.Type.Optional) WriteError!void {
    try beginType(stream, "optional");
    try stream.objectField("child");
    try streamType(stream, o.child);
}

fn beginType(stream: anytype, comptime name: []const u8) WriteError!void {
    try stream.objectField("type");
    try stream.write(name);
}

fn streamType(stream: anytype, comptime T: type) WriteError!void {
    try stream.beginObject();
    // catch special types pid, port and term
    switch (T) {
        e.ErlNifPid => {
            try beginType(stream, "pid");
        },
        beam.term => {
            try beginType(stream, "term");
        },
        e.ErlNifTerm => {
            try beginType(stream, "erl_nif_term");
        },
        beam.env => {
            try beginType(stream, "env");
        },
        else => {
            switch (@typeInfo(T)) {
                .Int => |i| try streamInt(stream, i),
                .Enum => |en| try streamEnum(stream, en, T),
                .Float => |f| try streamFloat(stream, f),
                .Struct => |s| try streamStruct(stream, s, T),
                .Array => |a| try streamArray(stream, a, std.fmt.comptimePrint("{}", .{T})),
                .Pointer => |p| try streamPointer(stream, p, std.fmt.comptimePrint("{}", .{T})),
                .Optional => |o| try streamOptional(stream, o),
                .Bool => try beginType(stream, "bool"),
                .Void => try beginType(stream, "void"),
                .ErrorUnion => |eu| {
                    try beginType(stream, "error");
                    try stream.objectField("child");
                    try streamType(stream, eu.payload);
                },
                else => {
                    try beginType(stream, "unusable:" ++ @typeName(T));
                },
            }
        },
    }
    try stream.endObject();
}

pub fn streamFun(stream: anytype, comptime name: anytype, comptime fun: std.builtin.Type.Fn) WriteError!void {
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

pub fn streamModule(stream: anytype, comptime Mod: type) WriteError!void {
    const mod_info = @typeInfo(Mod).Struct;
    try stream.beginObject();
    try stream.objectField("functions");
    try stream.beginArray();
    // functions are found in decls
    inline for (mod_info.decls) |decl| {
        const decl_info = @typeInfo(@TypeOf(@field(Mod, decl.name)));
        if (.Fn == decl_info) {
            try streamFun(stream, decl.name, decl_info.Fn);
        }
    }
    try stream.endArray();

    try stream.objectField("types");
    try stream.beginArray();
    // types are found in decls
    inline for (mod_info.decls) |decl| {
        switch (@typeInfo(@TypeOf(@field(Mod, decl.name)))) {
            .Type => {
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
            .Type => {},
            .Fn => {},
            else => {
                try stream.arrayElem();
                try stream.beginObject();
                try stream.objectField("name");
                try stream.emitString(decl.name);
                try stream.objectField("type");
                try stream.emitString(@typeName(@TypeOf(@field(Mod, decl.name))));
                try stream.endObject();
            },
        }
    }
    try stream.endArray();

    try stream.endObject();
}

pub fn main() WriteError!void {
    const stdout = std.io.getStdOut().writer();
    var stream = json.writeStream(stdout, .{});

    try streamModule(&stream, analyte);
}
