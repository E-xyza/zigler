pub fn Resource(comptime T: type) type {
    return struct {
        // we put this function in here to make sure it's got something special going on
        // and that it's not just a regular struct
        pub fn __wraps() type {
            return T;
        }
        resource_payload: T,
    };
}

fn Unwrap(comptime T: type) type {
    // returns the type wrapped by the resource
    switch (@typeInfo(T)) {
        .Struct => |s| {
            return s.fields[0].field_type;
        },
        else => @compileError("Resource.Unwrap should only be called on a resource type"),
    }
}

pub fn pack(data: anytype) Resource(@TypeOf(data)) {
    return .{ .resource_payload = data };
}

pub fn unpack(resource: anytype) Unwrap(@TypeOf(resource)) {
    return resource.resource_payload;
}
