// document for testing out the error parser
beam = @include("beam.zig")

// ref: /path/to/foo.ex line: 12

/// nif: foo/0
fn foo() i64 {
  return 47;
}

// ref: /path/to/foo.ex line: 45

/// nif: bar/0
fn bar() i64 {
  return 47;
}

// ref: test/unit/assets/error_parser_dummy.zig line: 18

// footer content

e.ErlNifFunc{
    .name = c"foo",
    .arity = 1,
    .fptr = __foo,
    .flags = 0,
},
};
const entry = e.ErlNifEntry{
    .major = 2,
    .minor = 15,
    .name = c"Elixir.ZiglerTest.ErrorParser",
    .num_of_funcs = 1,
    .funcs = &(exported_nifs[0]),
    .load = null,
    .reload = null,
    .upgrade = null,
    .unload = null,
    .vm_variant = c"beam.vanilla",
    .options = 1,
    .sizeof_ErlNifResourceTypeInit = 24,
    .min_erts = c"erts-10.4"
};

export fn nif_init() *const e.ErlNifEntry{
    return &entry;
}
