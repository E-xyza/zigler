-module(erlang_big_integer_test).
-compile({parse_transform, zigler}). 
-export([big_integer/1]).

-zig_code("
pub fn big_integer(value: u127) u127 {
    return value + 1;
}
").

-zig_opts([{otp_app, zigler}]).