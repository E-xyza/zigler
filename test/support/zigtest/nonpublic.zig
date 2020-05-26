const beam = @import("beam.zig");
const assert = beam.assert;

test "non-pub test" {
    assert(1 == 1);
}

pub fn foo() i32 {
    return 47;
}
