const StubType = struct { v: []const u8 };

const StubFunction = struct {
    name: []const u8,
    params: []const StubType,
    return_type: StubType,

    pub fn stream(comptime self: StubFunction, json_stream: anytype) !void {
        try json_stream.beginObject();

        // emit name
        try json_stream.objectField("name");
        try json_stream.write(self.name);

        // emit return type
        try json_stream.objectField("return");
        try json_stream.beginObject();
        try json_stream.objectField("type");
        try json_stream.write(self.return_type.v);
        try json_stream.endObject();

        // emit params
        try json_stream.objectField("params");
        try json_stream.beginArray();
        inline for (self.params) |param| {
            try json_stream.beginObject();
            try json_stream.objectField("type");
            try json_stream.write(param.v);
            try json_stream.endObject();
        }
        try json_stream.endArray();
        try json_stream.endObject();
    }
};

pub const functions = [_]StubFunction{
    // skip make_debug_allocator_instance because the type is too complicated
    .{ .name = "make_debug_allocator_instance", .params = &[_]StubType{}, .return_type = .{ .v = "unusable:std.heap.GeneralPurposeAllocator(...)" } },
    .{ .name = "get", .params = &[_]StubType{ .{ .v = "unusable:T" }, .{ .v = "unusable:beam.term" }, .{ .v = "unusable:anytype" } }, .return_type = .{ .v = "unusable:T" } },
};
