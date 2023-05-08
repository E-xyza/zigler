-module(erlang_big_integer_test).
-compile({parse_transform, zigler}). 
-export([add_one/1]).

-zig_code("
pub fn add_one(value: u127) u127 {
    return value + 1;
}
").

-zig_opts([{otp_app, zigler}]).