defmodule ZiglerTest.GeneratorTest do
  use ExUnit.Case, async: true

  # tests to make sure that the compiler makse the correct code.

  alias Zigler.{Module, Zig, Parser.Nif}

  @zeroarity %Nif{name: :foo, arity: 0, params: [], retval: "i64"}

  describe "the generator creates a reasonable shim" do
    test "for a single, zero arity function" do

      code = """
      // foo.exs line: 3

      fn foo() i64 {
        return 47;
      }
      """

      assert """
      const e = @cImport({
        @cInclude("erl_nif_zig.h");
      });

      const builtin = @import("builtin");
      const std = @import("std");
      const beam = @import("beam.zig");

      // foo.exs line: 3

      fn foo() i64 {
        return 47;
      }

      fn __foo_adapter__(env: beam.nev, argc: c_int, argv: [*c] const beam.term) beam.term {
        var result: c_int = foo();
        return beam.make_c_int(env, result);
      }

      extern fn __foo_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var res: beam.term = __foo_adapter__(env, argc, argv) catch | err | {
          if (err == beam.Error.FunctionClauseError) {
            return beam.throw_function_clause_error(env);
          } else if (err == error.OutOfMemory) {
            return beam.throw_enomem(env);
          } else {
            return e.enif_make_badarg(env);
          }
        };
        return res;
      }

      var exported_nifs = [1] e.ErlNifFunc{
        e.ErlNifFunc{
            .name = c"foo",
            .arity = 0,
            .fptr = __foo_shim__,
            .flags = 0,
        },
      };

      extern fn nif_load(env: beam.env, priv: [*c]?*c_void, load_info: beam.term) c_int {
         return 0;
      }

      const entry = e.ErlNifEntry{
          .major = 2,
          .minor = 15,
          .name = c"Elixir.Foo",
          .num_of_funcs = 1,
          .funcs = &(exported_nifs[0]),
          .load = nif_load,
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
      """ = Zig.generate(%Module{nifs: [@zeroarity], code: code, file: "foo.exs", module: Foo})
    end
  end
end
