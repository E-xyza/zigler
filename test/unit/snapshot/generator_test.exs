defmodule ZiglerTest.Snapshot.GeneratorTest do
  use ExUnit.Case, async: true

  # tests to make sure that the compiler makse the correct code.

  alias Zig.{Code, Module, Parser.Nif}

  @zeroarity %Nif{name: :foo, arity: 0, args: [], retval: "i64"}

  @moduletag :snapshot

  describe "the generator creates a reasonable shim" do
    test "for a single, zero arity function" do
      code = """
      // foo.exs line: 3

      fn foo() i64 {
        return 47;
      }
      """

      [major, minor] = Code.nif_major_minor

      assert """
      // foo.exs line: 3

      fn foo() i64 {
        return 47;
      }
      // ref: foo.zig line: 6

      // adapters for Elixir.Foo in foo.exs:

      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        beam.yield_info = null;

        var __foo_result__ = nosuspend foo();
        return beam.make_i64(env, __foo_result__);
      }

      // footer for Elixir.Foo in foo.exs:

      export var __exported_nifs__ = [_]e.ErlNifFunc{
        e.ErlNifFunc{
          .name = "foo",
          .arity = 0,
          .fptr = __foo_shim__,
          .flags = 0,
        },
      };

      const entry = e.ErlNifEntry{
        .major = #{major},
        .minor = #{minor},
        .name = "Elixir.Foo",
        .num_of_funcs = 1,
        .funcs = &(__exported_nifs__[0]),
        .load = beam.blank_load,
        .reload = beam.blank_load,     // currently unsupported
        .upgrade = beam.blank_upgrade, // currently unsupported
        .unload = beam.blank_unload,   // currently unsupported
        .vm_variant = "beam.vanilla",
        .options = 1,
        .sizeof_ErlNifResourceTypeInit = @sizeOf(e.ErlNifResourceTypeInit),
        .min_erts = "erts-#{:erlang.system_info(:version)}"
      };

      export fn nif_init() *const e.ErlNifEntry{
        return &entry;
      }
      """ == %Module{nifs: [@zeroarity], code: code, zig_file: "foo.zig",
                     file: "foo.exs", module: Foo, otp_app: :zigler}
             |> Code.generate_main
             |> IO.iodata_to_binary
    end

    @onearity %Nif{name: :foo, arity: 1, args: ["i64"], retval: "i64"}

    test "for a single, arity one function" do
      code = """
      // foo.exs line: 3

      fn foo(bar: i64) i64 {
        return bar;
      }
      """

      [major, minor] = Code.nif_major_minor

      assert """
      // foo.exs line: 3

      fn foo(bar: i64) i64 {
        return bar;
      }
      // ref: foo.zig line: 6

      // adapters for Elixir.Foo in foo.exs:

      export fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        beam.yield_info = null;

        var __foo_arg0__ = beam.get_i64(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        var __foo_result__ = nosuspend foo(__foo_arg0__);
        return beam.make_i64(env, __foo_result__);
      }

      // footer for Elixir.Foo in foo.exs:

      export var __exported_nifs__ = [_]e.ErlNifFunc{
        e.ErlNifFunc{
          .name = "foo",
          .arity = 1,
          .fptr = __foo_shim__,
          .flags = 0,
        },
      };

      const entry = e.ErlNifEntry{
        .major = #{major},
        .minor = #{minor},
        .name = "Elixir.Foo",
        .num_of_funcs = 1,
        .funcs = &(__exported_nifs__[0]),
        .load = beam.blank_load,
        .reload = beam.blank_load,     // currently unsupported
        .upgrade = beam.blank_upgrade, // currently unsupported
        .unload = beam.blank_unload,   // currently unsupported
        .vm_variant = "beam.vanilla",
        .options = 1,
        .sizeof_ErlNifResourceTypeInit = @sizeOf(e.ErlNifResourceTypeInit),
        .min_erts = "erts-#{:erlang.system_info(:version)}"
      };

      export fn nif_init() *const e.ErlNifEntry{
        return &entry;
      }
      """ == %Module{nifs: [@onearity], zig_file: "foo.zig", code: code,
                     file: "foo.exs", module: Foo, otp_app: :zigler}
             |> Code.generate_main
             |> IO.iodata_to_binary
    end

  end
end
