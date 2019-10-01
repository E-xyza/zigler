defmodule ZiglerTest.ZigAdlibTest do
  use ExUnit.Case

  alias Zigler.Zig

  test "nif_adapter produces expected content" do
    assert """
    extern fn __foo(env: ?*c.ErlNifEnv, argc: c_int, argv: [*c] const c.ErlNifTerm) c.ErlNifTerm {

    var arg0: c_int = 0;
    var arg1: c_int = 0;
    var res: c_int = 0;

    res = c.enif_get_int(env, argv[0], &arg0);
    res = c.enif_get_int(env, argv[1], &arg1);

    var result: c_int = foo(arg0, arg1);

    return c.enif_make_int(env, result);
    }
    """ == Zig.nif_adapter({:foo, {[:c_int, :c_int], :c_int}})
  end

  test "nif_header produces expected content" do
    assert """
    const c = @cImport({
    @cInclude("/foo/bar/baz.h");
    });
    """ == Zig.nif_header("/foo/bar/baz.h")
  end

  test "nif_footer produces expected content" do
  assert """
  const entry = c.ErlNifEntry{
      .major = 2,
      .minor = 15,
      .name = c"Elixir.FooBar",
      .num_of_funcs = 2,
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

  export fn nif_init() *const c.ErlNifEntry{
      return &entry;
  }
  """ == Zig.nif_footer(FooBar, [:foo, :bar])
  end

  test "nif_exports produces expected content" do
    assert """
    var exported_nifs = [2] c.ErlNifFunc{

    c.ErlNifFunc{
        .name = c"foo",
        .arity = 2,
        .fptr = __foo,
        .flags = 0,
    },
    c.ErlNifFunc{
        .name = c"bar",
        .arity = 3,
        .fptr = __bar,
        .flags = 0,
    },
    };
    """ == Zig.nif_exports(foo: {[:u64, :u64], :u64}, bar: {[:i64, :i64, :i64], :i64})
    end


end
