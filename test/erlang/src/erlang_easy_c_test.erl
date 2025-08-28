-module(erlang_easy_c_test).
-compile({parse_transform, zigler}). 
-export([cblas_daxpy/6]).

-zig_opts([
    {otp_app, zigler},
    {c, [{link_lib, {system, "blas"}}, {link_libc, true}]},
    {easy_c, "cblas.h"},
    {leak_check, true},
    {nifs, [
        {cblas_daxpy, [
            {params, #{4 => in_out}},
            {return, [{length, {arg, 0}}]}
        ]}
    ]}
]).