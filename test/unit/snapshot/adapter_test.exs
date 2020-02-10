defmodule ZiglerTest.Snapshot.AdapterTest do
  use ExUnit.Case, async: true

  alias Zigler.Code
  alias Zigler.Parser.Nif

  describe "for a basic, zero-arity function" do
    test "the shim function directly calls the target function" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_result__ = foo();
        return beam.make_c_long(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 0, params: [], retval: "c_long"}
      |> Code.adapter
    end

    test "the shim function can use other types" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_result__ = foo();
        return beam.make_c_int(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 0, params: [], retval: "c_int"}
      |> Code.adapter
    end
  end

end
