defmodule ZiglerTest.Unit.Typespec.ParamsTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  alias Zig.Type
  alias Zig.Type.Array
  alias Zig.Type.Bool
  alias Zig.Type.Cpointer
  alias Zig.Type.Function
  alias Zig.Type.Slice
  alias Zig.Type.Struct

  import Type, only: :macros

  def make_spec(type) do
    Function.spec(%Function{name: :params_test, arity: 1, params: [type], return: :void})
  end

  describe "when asking for a typespec return for basic types" do
    ###########################################################################
    ## INTS

    test "a u0-passed function is just zero" do
      result =
        quote context: Elixir do
          params_test(0) :: :ok
        end

      assert make_spec(~t(u0)) == result
    end

    test "a u8-passed function gives appropriate bounds" do
      result =
        quote context: Elixir do
          params_test(0..255) :: :ok
        end

      assert make_spec(~t(u8)) == result
    end

    test "a u16-passed function gives appropriate bounds" do
      result =
        quote context: Elixir do
          params_test(0..0xFFFF) :: :ok
        end

      assert make_spec(~t(u16)) == result
    end

    test "a u32-passed function gives appropriate bounds" do
      result =
        quote context: Elixir do
          params_test(0..0xFFFF_FFFF) :: :ok
        end

      assert make_spec(~t(u32)) == result
    end

    test "a u64-passed function gives non_neg_integer" do
      result =
        quote context: Elixir do
          params_test(0..0xFFFF_FFFF_FFFF_FFFF) :: :ok
        end

      assert make_spec(~t(u64)) == result
    end

    test "an i32-passed function gives appropriate bounds" do
      result =
        quote context: Elixir do
          params_test(-0x8000_0000..0x7FFF_FFFF) :: :ok
        end

      assert make_spec(~t(i32)) == result
    end

    test "an i64-passed function gives integer" do
      result =
        quote context: Elixir do
          params_test(-0x8000_0000_0000_0000..0x7FFF_FFFF_FFFF_FFFF) :: :ok
        end

      assert make_spec(~t(i64)) == result
    end

    # we're not going to test c_int, c_uint, c_long, usize, etc. because these are not
    # testable across platforms in an easy way, and zig will do the platform-dependent
    # translations at compile time

    ###########################################################################
    ## FLOATS

    test "an f16-passed function gives float" do
      result =
        quote context: Elixir do
          params_test(float()) :: :ok
        end

      assert make_spec(~t(f16)) == result
    end

    test "an f32-passed function gives float" do
      result =
        quote context: Elixir do
          params_test(float()) :: :ok
        end

      assert make_spec(~t(f32)) == result
    end

    test "an f64-passed function gives float" do
      result =
        quote context: Elixir do
          params_test(float()) :: :ok
        end

      assert make_spec(~t(f64)) == result
    end

    ###########################################################################
    ## BOOL

    test "a bool passed function is boolean" do
      result =
        quote context: Elixir do
          params_test(boolean()) :: :ok
        end

      assert make_spec(%Bool{}) == result
    end

    ###########################################################################
    ## BEAM

    test "a beam.term passed function is term" do
      result =
        quote context: Elixir do
          params_test(term()) :: :ok
        end

      assert make_spec(:term) == result
    end

    test "a e.ErlNifTerm passed function is term" do
      result =
        quote context: Elixir do
          params_test(term()) :: :ok
        end

      assert make_spec(:erl_nif_term) == result
    end

    test "a beam.pid passed function is pid" do
      result =
        quote context: Elixir do
          params_test(pid()) :: :ok
        end

      assert make_spec(:pid) == result
    end

    test "an enum passed function is just the optional atoms" do
      result =
        quote context: Elixir do
          params_test(:error | :maybe | :ok) :: :ok
        end

      param = %Zig.Type.Enum{tags: %{ok: "ok", error: "error", maybe: "maybe"}}

      assert make_spec(param) == result
    end
  end

  describe "when asking for function parameters for arraylike collections" do
    test "a u8-slice can be passed as binary or list" do
      result =
        quote context: Elixir do
          params_test([0..255] | binary()) :: :ok
        end

      assert make_spec(~t([]u8)) == result
    end

    test "a u8-slice with sentinel can be passed as binary or list" do
      result =
        quote context: Elixir do
          params_test([0..255] | binary()) :: :ok
        end

      assert make_spec(~t([:0]u8)) == result
    end

    test "a int-slice can be passed as list or binary" do
      result =
        quote context: Elixir do
          params_test([-0x8000_0000_0000_0000..0x7FFF_FFFF_FFFF_FFFF] | <<_::_*64>>) :: :ok
        end

      assert make_spec(~t([]i64)) == result
    end

    test "int-slice can be passed as list or binary with next biggest byte size" do
      result =
        quote context: Elixir do
          params_test([-0x4000_0000_0000_0000..0x3FFF_FFFF_FFFF_FFFF] | <<_::_*64>>) :: :ok
        end

      assert make_spec(~t([]i63)) == result
    end

    test "a float-slice passed function can be passed list or binary" do
      result =
        quote context: Elixir do
          params_test([float()] | <<_::_*32>>) :: :ok
        end

      assert make_spec(~t([]f32)) == result
    end

    test "manypointer without sentinel u8 can be passed as list or binary" do
      result =
        quote context: Elixir do
          params_test([0..255] | binary()) :: :ok
        end

      assert make_spec(~t([*]u8)) == result
    end

    test "manypointer with sentinel u8 can be passed list or binary" do
      result =
        quote context: Elixir do
          params_test([0..255] | binary()) :: :ok
        end

      assert make_spec(~t([*:0]u8)) == result
    end

    test "manypointer without sentinel u31 can be passed as list or binary" do
      result =
        quote context: Elixir do
          params_test([0..0x7FFF_FFFF] | <<_::_*32>>) :: :ok
        end

      assert make_spec(~t([*]u31)) == result
    end

    test "manypointer without sentinel f64 can be passed list or binary" do
      result =
        quote context: Elixir do
          params_test([float()] | <<_::_*64>>) :: :ok
        end

      assert make_spec(~t([*]f64)) == result
    end

    test "array with u8 without sentinel can be passed to binary or list" do
      result =
        quote context: Elixir do
          params_test([0..255] | <<_::80>>) :: :ok
        end

      assert make_spec(~t([10]u8)) == result
    end

    test "array with u8 with sentinel can be passed to binary or list" do
      result =
        quote context: Elixir do
          params_test([0..255] | binary()) :: :ok
        end

      assert make_spec(~t([10:0]u8)) == result
    end

    test "array with int can be passed binary or list of integer" do
      result =
        quote context: Elixir do
          params_test([0..0x7FFF_FFFF_FFFF_FFFF] | <<_::640>>) :: :ok
        end

      assert make_spec(~t([10]u63)) == result
    end

    test "array with float can be passed binary or list of float" do
      result =
        quote context: Elixir do
          params_test([float()] | <<_::320>>) :: :ok
        end

      assert make_spec(~t([10]f32)) == result
    end

    test "c pointer with u8 is assumed to be a string" do
      result =
        quote context: Elixir do
          params_test([0..255] | binary() | nil) :: :ok
        end

      assert make_spec(~t([*c]u8)) == result
    end

    test "c pointer pointer of u8 is assumed to be a null terminated list of strings" do
      result =
        quote context: Elixir do
          params_test([[0..255] | binary() | nil] | nil) :: :ok
        end

      assert make_spec(~t([*c][*c]u8)) == result
    end

    test "c pointer to a struct is assumed to be single struct" do
      result =
        quote context: Elixir do
          params_test((%{bar: 0..255} | [bar: 0..255]) | [%{bar: 0..255} | [bar: 0..255]] | nil) ::
            :ok
        end

      struct = %Struct{
        name: "Foo",
        required: %{bar: ~t(u8)},
        optional: %{},
        extern: true
      }

      assert make_spec(%Cpointer{child: struct}) == result
    end
  end

  describe "when asking for function parameter for structs" do
    test "it can be either a map or a keyword" do
      result =
        quote context: Elixir do
          params_test(
            %{optional(:bar) => [0..255] | binary(), foo: float()}
            | [bar: [0..255] | binary(), foo: float()]
          ) ::
            :ok
        end

      param = %Struct{
        name: "Foo",
        required: %{foo: ~t(f64)},
        optional: %{bar: ~t([]u8)}
      }

      assert make_spec(param) == result
    end

    @packed %Struct{
      name: "Foo",
      required: %{foo: ~t(f64)},
      optional: %{bar: ~t(u64)},
      packed: 16
    }

    test "it can also be a binary if it's packed" do
      result =
        quote context: Elixir do
          params_test(
            %{optional(:bar) => 0..18_446_744_073_709_551_615, foo: float()}
            | [bar: 0..18_446_744_073_709_551_615, foo: float()]
            | <<_::128>>
          ) :: :ok
        end

      assert make_spec(@packed) == result
    end

    test "slice of packeds is what you expect" do
      result =
        quote context: Elixir do
          params_test(
            [
              %{optional(:bar) => 0..18_446_744_073_709_551_615, foo: float()}
              | [bar: 0..18_446_744_073_709_551_615, foo: float()]
              | <<_::128>>
            ]
            | <<_::_*128>>
          ) :: :ok
        end

      assert make_spec(%Slice{child: @packed}) == result
    end

    test "array of packeds is what you expect" do
      result =
        quote context: Elixir do
          params_test(
            [
              %{optional(:bar) => 0..18_446_744_073_709_551_615, foo: float()}
              | [bar: 0..18_446_744_073_709_551_615, foo: float()]
              | <<_::128>>
            ]
            | <<_::256>>
          ) :: :ok
        end

      assert make_spec(%Array{child: @packed, len: 2}) == result
    end
  end

  describe "when asking for optional parameters" do
    test "it adds nil to the possible parameter" do
      result =
        quote context: Elixir do
          params_test(0..255 | nil) :: :ok
        end

      assert make_spec(~t(?u8)) == result
    end
  end

  describe "when asking for resource returns" do
    test "it marks it as a reference" do
      result =
        quote context: Elixir do
          params_test(reference()) :: :ok
        end

      assert make_spec(%Zig.Type.Resource{}) == result
    end
  end
end
