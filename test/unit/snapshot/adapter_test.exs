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
      |> IO.iodata_to_binary
    end

    test "the shim function can use other types" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_result__ = foo();

        return beam.make_c_int(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 0, params: [], retval: "c_int"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end
  end

  describe "for a one-arity function" do
    test "the shim function will correctly fill out parameters" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_i64(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        var __foo_result__ = foo(__foo_arg0__);

        return beam.make_i64(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 1, params: ["i64"], retval: "i64"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end
  end

  describe "for a zero-arity function with a environment term" do
    test "that is beam.env the shim function passes the env term in" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_result__ = foo(env);

        return beam.make_i64(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 0, params: ["beam.env"], retval: "i64"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "that is ?*e.ErlNifEnv the shim function passes the env term in" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_result__ = foo(env);

        return beam.make_i64(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 0, params: ["?*e.ErlNifEnv"], retval: "i64"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "the shim function respects beam.term type" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = argv[0];

        return foo(__foo_arg0__);
      }

      """ == %Nif{name: :foo, arity: 1, params: ["beam.term"], retval: "beam.term"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "the shim function respects e.ErlNifTerm type" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = argv[0];

        return foo(__foo_arg0__);
      }

      """ == %Nif{name: :foo, arity: 1, params: ["e.ErlNifTerm"], retval: "e.ErlNifTerm"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "the shim function respects beam.pid type" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_pid(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        foo(__foo_arg0__);

        return beam.make_nil(env);
      }

      """ == %Nif{name: :foo, arity: 1, params: ["beam.pid"], retval: "void"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "the shim function respects e.ErlNifPid type" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_pid(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        foo(__foo_arg0__);

        return beam.make_nil(env);
      }

      """ == %Nif{name: :foo, arity: 1, params: ["e.ErlNifPid"], retval: "void"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end
  end

  describe "for a one-arity function with a environment term" do
    test "that is beam.env the shim function passes the env term in" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_i64(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        var __foo_result__ = foo(env, __foo_arg0__);

        return beam.make_i64(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 1, params: ["beam.env", "i64"], retval: "i64"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "that is ?*e.ErlNifEnv the shim function passes the env term in" do
      assert """
      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_i64(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        var __foo_result__ = foo(env, __foo_arg0__);

        return beam.make_i64(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 1, params: ["?*e.ErlNifEnv", "i64"], retval: "i64"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end
  end

end
