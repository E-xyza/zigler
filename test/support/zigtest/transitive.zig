const beam = @import("beam.zig");
const assert = beam.assert;

pub fn forty_seven() i32 {
    return 47;
}

test "forty seven is forty seven" {
    assert(forty_seven() == 47);
}