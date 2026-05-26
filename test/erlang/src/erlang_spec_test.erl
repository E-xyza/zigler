-module(erlang_spec_test).
-compile({parse_transform, zigler}).
-export([add/2, get_bool/1, void_fn/0]).

-zig_code("
pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

pub fn get_bool(x: bool) bool {
    return x;
}

pub fn void_fn() void {
    return;
}
").

-zig_opts([{otp_app, zigler}]).
