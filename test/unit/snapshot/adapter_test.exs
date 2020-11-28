defmodule ZiglerTest.Snapshot.AdapterTest do
  use ExUnit.Case, async: true

  alias Zigler.Code
  alias Zigler.Parser.Nif

  describe "for a basic, zero-arity function" do
    test "the shim function directly calls the target function" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_result__ = foo();

        return beam.make_c_long(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 0, args: [], retval: "c_long"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "the shim function can use other types" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_result__ = foo();

        return beam.make_c_int(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 0, args: [], retval: "c_int"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end
  end

  describe "for a one-arity function" do
    test "the shim function will correctly fill out arguments" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_i64(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        var __foo_result__ = foo(__foo_arg0__);

        return beam.make_i64(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 1, args: ["i64"], retval: "i64"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end
  end

  describe "for a zero-arity function with a environment term" do
    test "that is beam.env the shim function passes the env term in" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_result__ = foo(env);

        return beam.make_i64(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 0, args: ["beam.env"], retval: "i64"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "that is ?*e.ErlNifEnv the shim function passes the env term in" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_result__ = foo(env);

        return beam.make_i64(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 0, args: ["?*e.ErlNifEnv"], retval: "i64"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end
  end

  describe "for a one-arity function with a special type" do
    test "the shim function respects beam.term type" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = argv[0];

        return foo(__foo_arg0__);
      }

      """ == %Nif{name: :foo, arity: 1, args: ["beam.term"], retval: "beam.term"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "the shim function respects e.ErlNifTerm type" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = argv[0];

        return foo(__foo_arg0__);
      }

      """ == %Nif{name: :foo, arity: 1, args: ["e.ErlNifTerm"], retval: "e.ErlNifTerm"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "the shim function respects beam.pid type" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_pid(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        foo(__foo_arg0__);

        return beam.make_nil(env);
      }

      """ == %Nif{name: :foo, arity: 1, args: ["beam.pid"], retval: "void"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "the shim function respects e.ErlNifPid type" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_pid(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        foo(__foo_arg0__);

        return beam.make_nil(env);
      }

      """ == %Nif{name: :foo, arity: 1, args: ["e.ErlNifPid"], retval: "void"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end
  end

  describe "for a one-arity function being passed a slice" do
    test "the shim function respects integers" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_slice_of(i32, env, argv[0]) catch |err| switch (err) {
          error.OutOfMemory => return beam.raise_enomem(env),
          beam.Error.FunctionClauseError => return beam.raise_function_clause_error(env)
        };
        defer beam.allocator.free(__foo_arg0__);

        var __foo_result__ = foo(__foo_arg0__);

        return beam.make_i32_list(env, __foo_result__) catch return beam.raise_enomem(env);
      }

      """ == %Nif{name: :foo, arity: 1, args: ["[]i32"], retval: "[]i32"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "the shim function respects floats" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_slice_of(f64, env, argv[0]) catch |err| switch (err) {
          error.OutOfMemory => return beam.raise_enomem(env),
          beam.Error.FunctionClauseError => return beam.raise_function_clause_error(env)
        };
        defer beam.allocator.free(__foo_arg0__);

        var __foo_result__ = foo(__foo_arg0__);

        return beam.make_f64_list(env, __foo_result__) catch return beam.raise_enomem(env);
      }

      """ == %Nif{name: :foo, arity: 1, args: ["[]f64"], retval: "[]f64"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end
  end

  describe "for a one-arity function with a environment term" do
    test "that is beam.env the shim function passes the env term in" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_i64(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        var __foo_result__ = foo(env, __foo_arg0__);

        return beam.make_i64(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 1, args: ["beam.env", "i64"], retval: "i64"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end

    test "that is ?*e.ErlNifEnv the shim function passes the env term in" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_i64(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        var __foo_result__ = foo(env, __foo_arg0__);

        return beam.make_i64(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 1, args: ["?*e.ErlNifEnv", "i64"], retval: "i64"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end
  end

  describe "for a one-arity function being passed a u8 slice" do
    test "the shim function assumes binary" do
      assert """
      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_char_slice(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        var __foo_result__ = foo(__foo_arg0__);

        return beam.make_slice(env, __foo_result__);
      }

      """ == %Nif{name: :foo, arity: 1, args: ["[]u8"], retval: "[]u8"}
      |> Code.adapter
      |> IO.iodata_to_binary
    end
  end
end
