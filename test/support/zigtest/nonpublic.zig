// note, this is not going to be imported as "public"
const std = @import("std");

test "imported test" {
    try std.testing.expect(1 == 1);
}

pub fn foo() i32 {
    return 47;
}
