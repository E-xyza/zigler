defmodule ZiglerTest.Unit.Typespec.ParamsTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  alias Zig.Nif
  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type
  alias Zig.Type.Array
  alias Zig.Type.Bool
  alias Zig.Type.Cpointer
  alias Zig.Type.Function
  alias Zig.Type.Slice
  alias Zig.Type.Struct

  import Type, only: :macros

  @tag :skip
  test "restore"

#  defp make_spec(type) do
#    %Nif{
#      name: :params_test,
#      export: true,
#      concurrency: Zig.Nif.Synchronous,
#      file: "foo.bar",
#      params: %{0 => %Parameter{type: type, cleanup: false}},
#      return: %Return{type: :void, cleanup: false}
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
#    ###########################################################################
#    ## INTS
#
#    test "a u0-passed function is just zero" do
#      assert make_spec(~t(u0)) == spec(params_test(0) :: :ok)
#    end
#
#    test "a u8-passed function gives appropriate bounds" do
#      assert make_spec(~t(u8)) == spec(params_test(0..255) :: :ok)
#    end
#
#    test "a u16-passed function gives appropriate bounds" do
#      assert make_spec(~t(u16)) == spec(params_test(0..0xFFFF) :: :ok)
#    end
#
#    test "a u32-passed function gives appropriate bounds" do
#      assert make_spec(~t(u32)) == spec(params_test(0..0xFFFF_FFFF) :: :ok)
#    end
#
#    test "a u64-passed function gives non_neg_integer" do
#      assert make_spec(~t(u64)) == spec(params_test(0..0xFFFF_FFFF_FFFF_FFFF) :: :ok)
#    end
#
#    test "an i32-passed function gives appropriate bounds" do
#      assert make_spec(~t(i32)) == spec(params_test(-0x8000_0000..0x7FFF_FFFF) :: :ok)
#    end
#
#    test "an i64-passed function gives integer" do
#      assert make_spec(~t(i64)) ==
#               spec(params_test(-0x8000_0000_0000_0000..0x7FFF_FFFF_FFFF_FFFF) :: :ok)
#    end
#
#    # we're not going to test c_int, c_uint, c_long, usize, etc. because these are not
#    # testable across platforms in an easy way, and zig will do the platform-dependent
#    # translations at compile time
#
#    ###########################################################################
#    ## FLOATS
#
#    test "an f16-passed function gives float" do
#      assert make_spec(~t(f16)) == spec(params_test(float()) :: :ok)
#    end
#
#    test "an f32-passed function gives float" do
#      assert make_spec(~t(f32)) == spec(params_test(float()) :: :ok)
#    end
#
#    test "an f64-passed function gives float" do
#      assert make_spec(~t(f64)) == spec(params_test(float()) :: :ok)
#    end
#
#    ###########################################################################
#    ## BOOL
#
#    test "a bool passed function is boolean" do
#      assert make_spec(%Bool{}) == spec(params_test(boolean()) :: :ok)
#    end
#
#    ###########################################################################
#    ## BEAM
#
#    test "a beam.term passed function is term" do
#      assert make_spec(:term) == spec(params_test(term()) :: :ok)
#    end
#
#    test "a e.ErlNifTerm passed function is term" do
#      assert make_spec(:erl_nif_term) == spec(params_test(term()) :: :ok)
#    end
#
#    test "a beam.pid passed function is pid" do
#      assert make_spec(:pid) == spec(params_test(pid()) :: :ok)
#    end
#
#    test "an enum passed function is just the optional atoms" do
#      param = %Zig.Type.Enum{tags: %{ok: "ok", error: "error", maybe: "maybe"}}
#
#      assert make_spec(param) == spec(params_test(:error | :maybe | :ok) :: :ok)
#    end
#  end
#
#  describe "when asking for function parameters for arraylike collections" do
#    test "a u8-slice can be passed as binary or list" do
#      assert make_spec(~t([]u8)) == spec(params_test([0..255] | binary()) :: :ok)
#    end
#
#    test "a u8-slice with sentinel can be passed as binary or list" do
#      assert make_spec(~t([:0]u8)) == spec(params_test([0..255] | binary()) :: :ok)
#    end
#
#    test "a int-slice can be passed as list or binary" do
#      assert make_spec(~t([]i64)) ==
#               spec(
#                 params_test([-0x8000_0000_0000_0000..0x7FFF_FFFF_FFFF_FFFF] | <<_::_*64>>) :: :ok
#               )
#    end
#
#    test "int-slice can be passed as list or binary with next biggest byte size" do
#      assert make_spec(~t([]i63)) ==
#               spec(
#                 params_test([-0x4000_0000_0000_0000..0x3FFF_FFFF_FFFF_FFFF] | <<_::_*64>>) :: :ok
#               )
#    end
#
#    test "a float-slice passed function can be passed list or binary" do
#      assert make_spec(~t([]f32)) == spec(params_test([float()] | <<_::_*32>>) :: :ok)
#    end
#
#    test "manypointer without sentinel u8 can be passed as list or binary" do
#      assert make_spec(~t([*]u8)) == spec(params_test([0..255] | binary()) :: :ok)
#    end
#
#    test "manypointer with sentinel u8 can be passed list or binary" do
#      assert make_spec(~t([*:0]u8)) == spec(params_test([0..255] | binary()) :: :ok)
#    end
#
#    test "manypointer without sentinel u31 can be passed as list or binary" do
#      assert make_spec(~t([*]u31)) == spec(params_test([0..0x7FFF_FFFF] | <<_::_*32>>) :: :ok)
#    end
#
#    test "manypointer without sentinel f64 can be passed list or binary" do
#      assert make_spec(~t([*]f64)) == spec(params_test([float()] | <<_::_*64>>) :: :ok)
#    end
#
#    test "array with u8 without sentinel can be passed to binary or list" do
#      assert make_spec(~t([10]u8)) == spec(params_test([0..255] | <<_::80>>) :: :ok)
#    end
#
#    test "array with u8 with sentinel can be passed to binary or list" do
#      assert make_spec(~t([10:0]u8)) == spec(params_test([0..255] | binary()) :: :ok)
#    end
#
#    test "array with int can be passed binary or list of integer" do
#      assert make_spec(~t([10]u63)) ==
#               spec(params_test([0..0x7FFF_FFFF_FFFF_FFFF] | <<_::640>>) :: :ok)
#    end
#
#    test "array with float can be passed binary or list of float" do
#      assert make_spec(~t([10]f32)) == spec(params_test([float()] | <<_::320>>) :: :ok)
#    end
#
#    test "c pointer with u8 is assumed to be a string" do
#      assert make_spec(~t([*c]u8)) == spec(params_test([0..255] | binary() | nil) :: :ok)
#    end
#
#    test "c pointer pointer of u8 is assumed to be a null terminated list of strings" do
#      assert make_spec(~t([*c][*c]u8)) ==
#               spec(params_test([[0..255] | binary() | nil] | nil) :: :ok)
#    end
#
#    test "c pointer to a struct is assumed to be single struct" do
#      struct = %Struct{
#        name: "Foo",
#        required: %{bar: ~t(u8)},
#        optional: %{},
#        extern: true
#      }
#
#      assert make_spec(%Cpointer{child: struct}) ==
#               spec(
#                 params_test(
#                   (%{bar: 0..255} | [bar: 0..255])
#                   | [%{bar: 0..255} | [bar: 0..255]]
#                   | nil
#                 ) ::
#                   :ok
#               )
#    end
#  end
#
#  describe "when asking for function parameter for structs" do
#    test "it can be either a map or a keyword" do
#      param = %Struct{
#        name: "Foo",
#        required: %{foo: ~t(f64)},
#        optional: %{bar: ~t([]u8)}
#      }
#
#      assert make_spec(param) ==
#               spec(
#                 params_test(
#                   %{optional(:bar) => [0..255] | binary(), foo: float()}
#                   | [bar: [0..255] | binary(), foo: float()]
#                 ) ::
#                   :ok
#               )
#    end
#
#    @packed %Struct{
#      name: "Foo",
#      required: %{foo: ~t(f64)},
#      optional: %{bar: ~t(u64)},
#      packed: 16
#    }
#
#    test "it can also be a binary if it's packed" do
#      assert make_spec(@packed) ==
#               spec(
#                 params_test(
#                   %{optional(:bar) => 0..18_446_744_073_709_551_615, foo: float()}
#                   | [bar: 0..18_446_744_073_709_551_615, foo: float()]
#                   | <<_::128>>
#                 ) :: :ok
#               )
#    end
#
#    test "slice of packeds is what you expect" do
#      assert make_spec(%Slice{child: @packed}) ==
#               spec(
#                 params_test(
#                   [
#                     %{optional(:bar) => 0..18_446_744_073_709_551_615, foo: float()}
#                     | [bar: 0..18_446_744_073_709_551_615, foo: float()]
#                     | <<_::128>>
#                   ]
#                   | <<_::_*128>>
#                 ) :: :ok
#               )
#    end
#
#    test "array of packeds is what you expect" do
#      assert make_spec(%Array{child: @packed, len: 2}) ==
#               spec(
#                 params_test(
#                   [
#                     %{optional(:bar) => 0..18_446_744_073_709_551_615, foo: float()}
#                     | [bar: 0..18_446_744_073_709_551_615, foo: float()]
#                     | <<_::128>>
#                   ]
#                   | <<_::256>>
#                 ) :: :ok
#               )
#    end
#  end
#
#  describe "when asking for optional parameters" do
#    test "it adds nil to the possible parameter" do
#      assert make_spec(~t(?u8)) == spec(params_test(0..255 | nil) :: :ok)
#    end
#  end
#
#  describe "when asking for resource returns" do
#    test "it marks it as a reference" do
#      assert make_spec(%Zig.Type.Resource{}) == spec(params_test(reference()) :: :ok)
#    end
#  end
end
