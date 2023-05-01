-module(test_zigler_erlang).
-compile({parse_transform, zigler}). 
-export([foo/1, foo/0]).

-zig_code("
pub fn foo() i32 {
    return 47;
}
").

-zig_opts([{otp_app, zigler}]).

<<<<<<< HEAD
-type value() :: integer().
-spec foo(value()) -> value().
=======
>>>>>>> 0.10.0-development
foo(X) ->
    47 + X.