defmodule ZiglerTest.Snapshot.GeneratorTest do
  use ExUnit.Case, async: true

  # tests to make sure that the compiler makse the correct code.

  alias Zigler.{Code, Module, Parser.Nif}

  @zeroarity %Nif{name: :foo, arity: 0, args: [], retval: "i64"}

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
      const e = @import("erl_nif.zig").c;
      const builtin = @import("builtin");
      const std = @import("std");
      const beam = @import("beam.zig");

      // foo.exs line: 3

      fn foo() i64 {
        return 47;
      }

      // ref: foo.zig line: 12

      // adapters for Elixir.Foo in foo.exs:

      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_result__ = foo();

        return beam.make_i64(env, __foo_result__);
      }

      // footer for Elixir.Foo in foo.exs:

      var __exported_nifs__ = [_] e.ErlNifFunc{
        e.ErlNifFunc{
          .name = c"foo",
          .arity = 0,
          .fptr = __foo_shim__,
          .flags = 0,
        },
      };

      const entry = e.ErlNifEntry{
        .major = #{major},
        .minor = #{minor},
        .name = c"Elixir.Foo",
        .num_of_funcs = 1,
        .funcs = &(__exported_nifs__[0]),
        .load = null,
        .reload = null,
        .upgrade = null,
        .unload = null,
        .vm_variant = c"beam.vanilla",
        .options = 1,
        .sizeof_ErlNifResourceTypeInit = 24,
        .min_erts = c"erts-#{:erlang.system_info(:version)}"
      };

      export fn nif_init() *const e.ErlNifEntry{
        return &entry;
      }
      """ == %Module{nifs: [@zeroarity], code: code, zig_file: "foo.zig",
                     file: "foo.exs", module: Foo, app: :zigler}
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
      const e = @import("erl_nif.zig").c;
      const builtin = @import("builtin");
      const std = @import("std");
      const beam = @import("beam.zig");

      // foo.exs line: 3

      fn foo(bar: i64) i64 {
        return bar;
      }

      // ref: foo.zig line: 12

      // adapters for Elixir.Foo in foo.exs:

      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var __foo_arg0__ = beam.get_i64(env, argv[0])
          catch return beam.raise_function_clause_error(env);

        var __foo_result__ = foo(__foo_arg0__);

        return beam.make_i64(env, __foo_result__);
      }

      // footer for Elixir.Foo in foo.exs:

      var __exported_nifs__ = [_] e.ErlNifFunc{
        e.ErlNifFunc{
          .name = c"foo",
          .arity = 1,
          .fptr = __foo_shim__,
          .flags = 0,
        },
      };

      const entry = e.ErlNifEntry{
        .major = #{major},
        .minor = #{minor},
        .name = c"Elixir.Foo",
        .num_of_funcs = 1,
        .funcs = &(__exported_nifs__[0]),
        .load = null,
        .reload = null,
        .upgrade = null,
        .unload = null,
        .vm_variant = c"beam.vanilla",
        .options = 1,
        .sizeof_ErlNifResourceTypeInit = 24,
        .min_erts = c"erts-#{:erlang.system_info(:version)}"
      };

      export fn nif_init() *const e.ErlNifEntry{
        return &entry;
      }
      """ == %Module{nifs: [@onearity], zig_file: "foo.zig", code: code,
                     file: "foo.exs", module: Foo, app: :zigler}
             |> Code.generate_main
             |> IO.iodata_to_binary
    end

  end
end
