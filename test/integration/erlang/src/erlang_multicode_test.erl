-module(erlang_multicode_test).
-compile({parse_transform, zigler}). 
-export([part1/0, part2/0]).

-zig_code("
pub fn part1() i32 {
    return 47;
}
").

-zig_opts([{otp_app, zigler}, {nifs, auto}]).

-zig_code("
pub fn part2() i32 {
    return 48;
}").