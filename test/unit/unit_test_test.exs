defmodule ZiglerTest.UnitTestTest do
  use ExUnit.Case, async: true

  @this_file __ENV__.file

  test "`use Zig.Unit` requires `use Zig`" do
    assert_raise CompileError, fn ->
      @this_file
      |> Path.dirname
      |> Path.join("assets/unit_error/no_zigler_error.exs")
      |> Code.compile_file
    end
  end

  alias Zig.Parser.Nif
  test "Zig.Code.adapter/1 produces the correct adapter for a test" do
    assert """
    export fn __test_foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
      beam.test_env = env;
      test_foo() catch return beam.test_error();
      return beam.make_atom(env, "ok");
    }
    """ = %Nif{name: :test_foo, arity: 0, test: "tests foo"}
    |> Zig.Code.adapter(__MODULE__)
    |> IO.iodata_to_binary
  end

  test "Zig.Code.nif_table_entries/1 produces the correct skeleton for a test" do
    assert """
      e.ErlNifFunc{
        .name = "tests foo",
        .arity = 0,
        .fptr = __test_foo_shim__,
        .flags = 0,
      },
    """ = %Nif{name: :test_foo, arity: 0, test: "tests foo"}
    |> Zig.Code.nif_table_entries
    |> IO.iodata_to_binary
  end
end
