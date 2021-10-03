const std = @import("std");

test "imported test" {
    try std.testing.expect(1 == 1);
}
