-module(erlang_erroring_test).
-compile({parse_transform, zigler}). 
-export([errors/0]).

-zig_opts([{otp_app, zigler}]).
-zig_code("
const MyError = error{some_error};

pub fn errors() !void {
    return error.some_error;
}
").
