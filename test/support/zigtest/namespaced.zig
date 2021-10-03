const std = @import("std");

test "namespaced test" {
    try std.testing.expect(1 == 1);
}
