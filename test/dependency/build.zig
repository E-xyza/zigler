const std = @import("std");

pub fn build(b: *std.Build) void {
    const resolved_target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("mymod", .{
        .root_source_file = .{ .src_path = .{.owner = b, .sub_path = "src/lib.zig"} },
        .target = resolved_target,
        .optimize = optimize
    });
}
