defmodule ZiglerTest.Snapshot.FooterTest do
  use ExUnit.Case, async: true

  alias Zigler.{Module, Parser.Nif, Code}

  describe "the zigler compiler footer generates" do
    test "works for a single function" do

      [major, minor] = Code.nif_major_minor()

      assert """
      var exported_nifs = [1] e.ErlNifFunc{
        e.ErlNifFunc{
          .name = c"foo",
          .arity = 0,
          .fptr = __foo_shim__,
          .flags = 0,
        },
      };

      export fn nif_load(env: beam.env, priv: [*c]?*c_void, load_info: beam.term) c_int {
        return 0;
      }

      const entry = e.ErlNifEntry{
        .major = #{major},
        .minor = #{minor},
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
        .min_erts = c"erts-#{:erlang.system_info(:version)}"
      };

      export fn nif_init() *const e.ErlNifEntry{
        return &entry;
      }
      """ == %Module{nifs: [%Nif{name: :foo, arity: 0}], file: "foo.exs", module: Foo, app: :zigler}
      |> Code.footer
      |> IO.iodata_to_binary
    end

    test "works for multiple functions" do

      [major, minor] = Code.nif_major_minor()

      assert """
      var exported_nifs = [2] e.ErlNifFunc{
        e.ErlNifFunc{
          .name = c"foo",
          .arity = 0,
          .fptr = __foo_shim__,
          .flags = 0,
        },
        e.ErlNifFunc{
          .name = c"bar",
          .arity = 1,
          .fptr = __bar_shim__,
          .flags = 0,
        },
      };

      export fn nif_load(env: beam.env, priv: [*c]?*c_void, load_info: beam.term) c_int {
        return 0;
      }

      const entry = e.ErlNifEntry{
        .major = #{major},
        .minor = #{minor},
        .name = c"Elixir.Baz",
        .num_of_funcs = 2,
        .funcs = &(exported_nifs[0]),
        .load = nif_load,
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
      """ == %Module{nifs: [
               %Nif{name: :foo, arity: 0},
               %Nif{name: :bar, arity: 1}],
             file: "foo.exs", module: Baz, app: :zigler}
      |> Code.footer
      |> IO.iodata_to_binary
    end
  end
end
