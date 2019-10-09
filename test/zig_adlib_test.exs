defmodule ZiglerTest.ZigAdlibTest do
  use ExUnit.Case

  alias Zigler.Zig

  test "nif_adapter produces expected content" do
    assert """
    extern fn __foo(env: ?*e.ErlNifEnv, argc: c_int, argv: [*c] const e.ErlNifTerm) e.ErlNifTerm {

    var arg0: c_int = undefined;
    var arg1: c_int = undefined;
    var res: c_int = 0;
      arg0 = beam.get_c_int(env, argv[0]) catch {
        return beam.throw_function_clause_error(env);
      };

      arg1 = beam.get_c_int(env, argv[1]) catch {
        return beam.throw_function_clause_error(env);
      };


    var result: c_int = foo(arg0, arg1);

    return e.enif_make_int(env, result);
    }
    """ == Zig.nif_adapter({:foo, {[:c_int, :c_int], :c_int}})
  end

  test "nif_header produces expected content" do
    assert """
    // import a header containing all of the NIF ABI forwards.
    const e = @import("erl_nif.zig").c;
    const beam = @import("beam.zig");
    """ == Zig.nif_header()
  end

  test "nif_footer produces expected content" do
  assert """
  const entry = e.ErlNifEntry{
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

  export fn nif_init() *const e.ErlNifEntry{
      return &entry;
  }
  """ == Zig.nif_footer(FooBar, [:foo, :bar])
  end

  test "nif_exports produces expected content" do
    assert """
    var exported_nifs = [2] e.ErlNifFunc{

    e.ErlNifFunc{
        .name = c"foo",
        .arity = 2,
        .fptr = __foo,
        .flags = 0,
    },
    e.ErlNifFunc{
        .name = c"bar",
        .arity = 3,
        .fptr = __bar,
        .flags = 0,
    },
    };
    """ == Zig.nif_exports(foo: {[:u64, :u64], :u64}, bar: {[:i64, :i64, :i64], :i64})
  end

  test "nif_exports strips ?*ErlNifEnv from arity" do
    assert """
    var exported_nifs = [1] e.ErlNifFunc{

    e.ErlNifFunc{
        .name = c"foo",
        .arity = 2,
        .fptr = __foo,
        .flags = 0,
    },
    };
    """ == Zig.nif_exports(foo: {[:"?*e.ErlNifEnv", :u64, :u64], :u64})
  end
end
