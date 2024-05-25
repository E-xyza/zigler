defmodule ZiglerTest.Unit.Typespec.ReturnTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  alias Zig.Nif
  alias Zig.Return
  alias Zig.Type
  alias Zig.Type.Array
  alias Zig.Type.Bool
  alias Zig.Type.Function
  alias Zig.Type.Slice
  alias Zig.Type.Struct

  import Type, only: :macros

  @tag :skip
  test "restore this"

  #  defp make_spec(type, as \\ :default) do
  #    %Nif{
  #      name: :return_test,
  #      export: true,
  #      concurrency: Zig.Nif.Synchronous,
  #      file: "foo.bar",
  #      params: %{},
  #      return: %Return{type: type, cleanup: false, as: as}
  #    }
  #    |> Nif.render_elixir_spec()
  #    |> scrub
  #  end
  #
  #  defp scrub(spec) do
  #    Macro.postwalk(spec, fn
  #      {a, _, nil} -> {a, [], Elixir}
  #      {a, _, c} -> {a, [], c}
  #      other -> other
  #    end)
  #  end
  #
  #  defmacro spec(spec) do
  #    Macro.escape(
  #      quote context: Elixir do
  #        unquote(scrub(spec))
  #      end
  #    )
  #  end
  #
  #  describe "when asking for a typespec return for basic types" do
  #    test "a void function gives a sane result" do
  #      assert make_spec(:void) == spec(return_test() :: :ok)
  #    end
  #
  #    ###########################################################################
  #    ## INTS
  #
  #    test "a u0-returning function gives appropriate bounds" do
  #      assert make_spec(~t(u0)) == spec(return_test() :: 0)
  #    end
  #
  #    test "a u8-returning function gives appropriate bounds" do
  #      assert make_spec(~t(u8)) == spec(return_test() :: 0..255)
  #    end
  #
  #    test "a u16-returning function gives appropriate bounds" do
  #      assert make_spec(~t(u16)) == spec(return_test() :: 0..0xFFFF)
  #    end
  #
  #    test "a u32-returning function gives appropriate bounds" do
  #      assert make_spec(~t(u32)) == spec(return_test() :: 0..0xFFFF_FFFF)
  #    end
  #
  #    test "a u64-returning function gives bounds" do
  #      assert make_spec(~t(u64)) == spec(return_test() :: 0..0xFFFF_FFFF_FFFF_FFFF)
  #    end
  #
  #    test "an i32-returning function gives appropriate bounds" do
  #      assert make_spec(~t(i32)) == spec(return_test() :: -0x8000_0000..0x7FFF_FFFF)
  #    end
  #
  #    test "an i64-returning function gives integer" do
  #      assert make_spec(~t(i63)) ==
  #               spec(return_test() :: -0x4000_0000_0000_0000..0x3FFF_FFFF_FFFF_FFFF)
  #    end
  #
  #    # we're not going to test c_int, c_uint, c_long, usize, etc. because these are not
  #    # testable across platforms in an easy way, and zig will do the platform-dependent
  #    # translations at compile time
  #
  #    ###########################################################################
  #    ## FLOATS
  #
  #    test "an f16-returning function gives float" do
  #      assert make_spec(~t(f16)) == spec(return_test() :: float())
  #    end
  #
  #    test "an f32-returning function gives float" do
  #      assert make_spec(~t(f32)) == spec(return_test() :: float())
  #    end
  #
  #    test "an f64-returning function gives float" do
  #      assert make_spec(~t(f64)) == spec(return_test() :: float())
  #    end
  #
  #    ###########################################################################
  #    ## BOOL
  #
  #    test "a bool returning function is boolean" do
  #      assert make_spec(%Bool{}) == spec(return_test() :: boolean())
  #    end
  #
  #    ###########################################################################
  #    ## BEAM
  #
  #    test "a beam.term returning function is term" do
  #      assert make_spec(:term) == spec(return_test() :: term())
  #    end
  #
  #    test "a e.ErlNifTerm returning function is term" do
  #      assert make_spec(:erl_nif_term) == spec(return_test() :: term())
  #    end
  #
  #    test "a beam.pid returning function is pid" do
  #      assert make_spec(:pid) == spec(return_test() :: pid())
  #    end
  #
  #    test "an enum returning function is just the optional atoms" do
  #      return = %Zig.Type.Enum{tags: %{ok: "ok", error: "error", maybe: "maybe"}}
  #      assert make_spec(return) == spec(return_test() :: :error | :maybe | :ok)
  #    end
  #  end
  #
  #  describe "when asking for function returns for arraylike collections" do
  #    test "a u8-slice returning function is special and defaults to binary" do
  #      assert make_spec(~t([]u8)) == spec(return_test() :: binary())
  #    end
  #
  #    test "u8-slice can be forced to return list" do
  #      assert make_spec(~t([]u8), :list) == spec(return_test() :: [0..255])
  #    end
  #
  #    test "a int-slice returning function is list of integer" do
  #      assert make_spec(~t([]i64)) ==
  #               spec(return_test() :: [-0x8000_0000_0000_0000..0x7FFF_FFFF_FFFF_FFFF])
  #    end
  #
  #    test "int-slice can be forced to return binary" do
  #      assert make_spec(~t([]i64), :binary) == spec(return_test() :: <<_::_*64>>)
  #    end
  #
  #    test "int-slice will pack as the biggest power of two size" do
  #      assert make_spec(~t([]i63), :binary) == spec(return_test() :: <<_::_*64>>)
  #    end
  #
  #    test "a float-slice returning function is list of float" do
  #      assert make_spec(~t([]f64)) == spec(return_test() :: [float()])
  #    end
  #
  #    test "float-slice can be forced to return binary" do
  #      assert make_spec(~t([]f32), :binary) == spec(return_test() :: <<_::_*32>>)
  #    end
  #
  #    test "manypointer with sentinel u8 defaults to binary" do
  #      assert make_spec(~t([*:0]u8)) == spec(return_test() :: binary())
  #    end
  #
  #    test "manypointer with sentinel u8 can be list" do
  #      assert make_spec(~t([*:0]u8), :list) == spec(return_test() :: [0..255])
  #    end
  #
  #    test "array with u8 defaults to binary" do
  #      assert make_spec(~t([10]u8)) == spec(return_test() :: <<_::80>>)
  #    end
  #
  #    test "array with u8 can be forced to return list" do
  #      assert make_spec(~t([10]u8), :list) == spec(return_test() :: [0..255])
  #    end
  #
  #    test "array with int defaults to list of integer" do
  #      assert make_spec(~t([10]u64)) == spec(return_test() :: [0..0xFFFF_FFFF_FFFF_FFFF])
  #    end
  #
  #    test "array with int can be forced to return binary" do
  #      assert make_spec(~t([10]u64), :binary) == spec(return_test() :: <<_::640>>)
  #    end
  #
  #    test "array with int, unusual size can be forced to return binary" do
  #      assert make_spec(~t([10]u63), :binary) == spec(return_test() :: <<_::640>>)
  #    end
  #
  #    test "array with float defaults to list of float" do
  #      assert make_spec(~t([10]f64)) == spec(return_test() :: [float()])
  #    end
  #
  #    test "array with float can be forced to return binary" do
  #      assert make_spec(~t([10]f32), :binary) == spec(return_test() :: <<_::320>>)
  #    end
  #
  #    test "c pointer with u8 is assumed to be a string" do
  #      assert make_spec(~t([*c]u8)) == spec(return_test() :: binary())
  #    end
  #
  #    test "c pointer with u8 is assumed to be a string and can be turned into a list" do
  #      assert make_spec(~t([*c]u8), :list) == spec(return_test() :: [0..255])
  #    end
  #
  #    test "c pointer pointer of u8 is assumed to be a null terminated list of strings" do
  #      assert make_spec(~t([*c][*c]u8)) == spec(return_test() :: [binary()])
  #    end
  #
  #    test "c pointer to a struct is assumed to be single struct" do
  #      cpointer_struct = %Struct{
  #        name: "Foo",
  #        required: %{bar: ~t(u8)},
  #        optional: %{},
  #        extern: true
  #      }
  #
  #      assert make_spec(cpointer_struct) == spec(return_test() :: %{bar: 0..255})
  #    end
  #  end
  #
  #  describe "when asking for function returns for structs" do
  #    test "it returns a straight map" do
  #      return = %Struct{
  #        name: "Foo",
  #        required: %{foo: ~t(f64)},
  #        optional: %{bar: ~t([]u8)}
  #      }
  #
  #      assert make_spec(return) == spec(return_test() :: %{bar: binary(), foo: float()})
  #    end
  #
  #    @packed %Struct{
  #      name: "Foo",
  #      required: %{foo: ~t(f64)},
  #      optional: %{bar: ~t(u64)},
  #      packed: 16
  #    }
  #
  #    test "it returns binary if it's packed" do
  #      assert make_spec(@packed, :binary) == spec(return_test() :: <<_::128>>)
  #    end
  #
  #    test "slice of packeds is what you expect" do
  #      return = %Slice{child: @packed}
  #      assert make_spec(return, :binary) == spec(return_test() :: <<_::_*128>>)
  #    end
  #
  #    test "array of packeds is what you expect" do
  #      return = %Array{child: @packed, len: 2, has_sentinel?: false}
  #
  #      assert make_spec(return, :binary) == spec(return_test() :: <<_::256>>)
  #    end
  #  end
  #
  #  describe "when asking for optional returns" do
  #    test "it adds nil to the possible return" do
  #      assert make_spec(~t(?u8)) == spec(return_test() :: 0..255 | nil)
  #    end
  #  end
  #
  #  describe "when asking for resource returns" do
  #    test "it marks it as a reference" do
  #      return = %Zig.Type.Resource{}
  #
  #      assert make_spec(return) == spec(return_test() :: reference())
  #    end
  #
  #    test "it can know if the resource will emerge as a binary" do
  #      return = %Zig.Type.Resource{}
  #
  #      assert make_spec(return, :binary) == spec(return_test() :: binary())
  #    end
  #  end
  #
  #  describe "when there's an in-out parameter for easy_c" do
  #    @tag :skip
  #    # do
  #    test "it works when the length is specified"
  #    #  assert Spec.for(
  #    #           %Function{
  #    #             name: :return_test,
  #    #             arity: 0,
  #    #             params: [~t([*c]u8)],
  #    #             return: :void
  #    #           },
  #    #           return: [:default, length: 2, arg: 0]
  #    #         ) == spec(return_test([0..255] | binary() | nil) :: <<_::16>>)
  #    # end
  #
  #    @tag :skip
  #    # do
  #    test "it works when the length is not specified"
  #    #  result =
  #    #    quote context: Elixir do
  #    #      return_test([0..255] | binary() | nil) :: binary()
  #    #    end
  #    #
  #    #  assert Spec.for(
  #    #           %Function{
  #    #             name: :return_test,
  #    #             arity: 0,
  #    #             params: [~t([*c]u8)],
  #    #             return: :void
  #    #           },
  #    #           return: [:default, arg: 0]
  #    #         ) == result
  #    # end
  #  end
end
