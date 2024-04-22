-module(erlang_easy_c_test).
-compile({parse_transform, zigler}). 
-export([cblas_daxpy/6]).

-zig_opts([
    {otp_app, zigler},
    {link_lib, {system, "blas"}},
    {easy_c, "cblas.h"},
    {leak_check, false},
    {nifs, [{cblas_daxpy, [{return, [4, {length, {arg, 0}}]}]}]}
]).