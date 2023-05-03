defmodule ZiglerTest.Unit.Typespec.ReturnTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  alias Zig.Type
  alias Zig.Type.Array
  alias Zig.Type.Bool
  alias Zig.Type.Function
  alias Zig.Type.Slice
  alias Zig.Type.Struct

  import Type, only: :macros

  def make_spec(type, opts \\ [type: :default]) do
    Function.spec(%Function{
      name: :return_test,
      arity: 0,
      params: [],
      return: type,
      opts: [return: opts]
    })
  end

  describe "when asking for a typespec return for basic types" do
    test "a void function gives a sane result" do
      result =
        quote context: Elixir do
          return_test() :: :ok
        end

      assert make_spec(:void) == result
    end

    ###########################################################################
    ## INTS

    test "a u8-returning function gives appropriate bounds" do
      result =
        quote context: Elixir do
          return_test() :: 0..255
        end

      assert make_spec(~t(u8)) == result
    end

    test "a u16-returning function gives appropriate bounds" do
      result =
        quote context: Elixir do
          return_test() :: 0..0xFFFF
        end

      assert make_spec(~t(u16)) == result
    end

    test "a u32-returning function gives appropriate bounds" do
      result =
        quote context: Elixir do
          return_test() :: 0..0xFFFF_FFFF
        end

      assert make_spec(~t(u32)) == result
    end

    test "a u64-returning function gives bounds" do
      result =
        quote context: Elixir do
          return_test() :: 0..0xFFFF_FFFF_FFFF_FFFF
        end

      assert make_spec(~t(u64)) == result
    end

    test "an i32-returning function gives appropriate bounds" do
      result =
        quote context: Elixir do
          return_test() :: -0x8000_0000..0x7FFF_FFFF
        end

      assert make_spec(~t(i32)) == result
    end

    test "an i64-returning function gives integer" do
      result =
        quote context: Elixir do
          return_test() :: -0x4000_0000_0000_0000..0x3FFF_FFFF_FFFF_FFFF
        end

      assert make_spec(~t(i63)) == result
    end

    # we're not going to test c_int, c_uint, c_long, usize, etc. because these are not
    # testable across platforms in an easy way, and zig will do the platform-dependent
    # translations at compile time

    ###########################################################################
    ## FLOATS

    test "an f16-returning function gives float" do
      result =
        quote context: Elixir do
          return_test() :: float()
        end

      assert make_spec(~t(f16)) == result
    end

    test "an f32-returning function gives float" do
      result =
        quote context: Elixir do
          return_test() :: float()
        end

      assert make_spec(~t(f32)) == result
    end

    test "an f64-returning function gives float" do
      result =
        quote context: Elixir do
          return_test() :: float()
        end

      assert make_spec(~t(f64)) == result
    end

    ###########################################################################
    ## BOOL

    test "a bool returning function is boolean" do
      result =
        quote context: Elixir do
          return_test() :: boolean()
        end

      assert make_spec(%Bool{}) == result
    end

    ###########################################################################
    ## BEAM

    test "a beam.term returning function is term" do
      result =
        quote context: Elixir do
          return_test() :: term()
        end

      assert make_spec(:term) == result
    end

    test "a e.ErlNifTerm returning function is term" do
      result =
        quote context: Elixir do
          return_test() :: term()
        end

      assert make_spec(:erl_nif_term) == result
    end

    test "a beam.pid returning function is pid" do
      result =
        quote context: Elixir do
          return_test() :: pid()
        end

      assert make_spec(:pid) == result
    end

    test "an enum returning function is just the optional atoms" do
      result =
        quote context: Elixir do
          return_test() :: :error | :maybe | :ok
        end

      return = %Zig.Type.Enum{tags: %{ok: "ok", error: "error", maybe: "maybe"}}

      assert make_spec(return) == result
    end
  end

  describe "when asking for function returns for arraylike collections" do
    test "a u8-slice returning function is special and defaults to binary" do
      result =
        quote context: Elixir do
          return_test() :: binary()
        end

      assert make_spec(~t([]u8)) == result
    end

    test "u8-slice can be forced to return list" do
      result =
        quote context: Elixir do
          return_test() :: [0..255]
        end

      assert make_spec(~t([]u8), type: :charlist) == result
    end

    test "a int-slice returning function is list of integer" do
      result =
        quote context: Elixir do
          return_test() :: [-0x8000_0000_0000_0000..0x7FFF_FFFF_FFFF_FFFF]
        end

      assert make_spec(~t([]i64)) == result
    end

    test "int-slice can be forced to return binary" do
      result =
        quote context: Elixir do
          return_test() :: <<_::_*64>>
        end

      assert make_spec(~t([]i64), type: :binary) == result
    end

    test "int-slice will pack as the biggest power of two size" do
      result =
        quote context: Elixir do
          return_test() :: <<_::_*64>>
        end

      assert make_spec(~t([]i63), type: :binary) == result
    end

    test "a float-slice returning function is list of float" do
      result =
        quote context: Elixir do
          return_test() :: [float()]
        end

      assert make_spec(~t([]f64)) == result
    end

    test "float-slice can be forced to return binary" do
      result =
        quote context: Elixir do
          return_test() :: <<_::_*32>>
        end

      assert make_spec(~t([]f32), type: :binary) == result
    end

    test "manypointer with sentinel u8 defaults to binary" do
      result =
        quote context: Elixir do
          return_test() :: binary()
        end

      assert make_spec(~t([*:0]u8)) == result
    end

    test "manypointer with sentinel u8 can be charlist" do
      result =
        quote context: Elixir do
          return_test() :: [0..255]
        end

      assert make_spec(~t([*:0]u8), type: :charlist) == result
    end

    test "array with u8 defaults to binary" do
      result =
        quote context: Elixir do
          return_test() :: <<_::80>>
        end

      assert make_spec(~t([10]u8)) == result
    end

    test "array with u8 can be forced to return charlist" do
      result =
        quote context: Elixir do
          return_test() :: [0..255]
        end

      assert make_spec(~t([10]u8), type: :charlist) == result
    end

    test "array with int defaults to list of integer" do
      result =
        quote context: Elixir do
          return_test() :: [0..0xFFFF_FFFF_FFFF_FFFF]
        end

      assert make_spec(~t([10]u64)) == result
    end

    test "array with int can be forced to return binary" do
      result =
        quote context: Elixir do
          return_test() :: <<_::640>>
        end

      assert make_spec(~t([10]u64), type: :binary) == result
    end

    test "array with int, unusual size can be forced to return binary" do
      result =
        quote context: Elixir do
          return_test() :: <<_::640>>
        end

      assert make_spec(~t([10]u63), type: :binary) == result
    end

    test "array with float defaults to list of float" do
      result =
        quote context: Elixir do
          return_test() :: [float()]
        end

      assert make_spec(~t([10]f64)) == result
    end

    test "array with float can be forced to return binary" do
      result =
        quote context: Elixir do
          return_test() :: <<_::320>>
        end

      assert make_spec(~t([10]f32), type: :binary) == result
    end

    test "c pointer with u8 is assumed to be a string" do
      result =
        quote context: Elixir do
          return_test() :: binary()
        end

      assert make_spec(~t([*c]u8)) == result
    end

    test "c pointer with u8 is assumed to be a string and can be turned into a charlist" do
      result =
        quote context: Elixir do
          return_test() :: [0..255]
        end

      assert make_spec(~t([*c]u8), type: :charlist) == result
    end

    test "c pointer pointer of u8 is assumed to be a null terminated list of strings" do
      result =
        quote context: Elixir do
          return_test() :: [binary()]
        end

      assert make_spec(~t([*c][*c]u8)) == result
    end

    test "c pointer to a struct is assumed to be single struct" do
      result =
        quote context: Elixir do
          return_test() :: %{bar: 0..255}
        end

      cpointer_struct = %Struct{
        name: "Foo",
        required: %{bar: ~t(u8)},
        optional: %{},
        extern: true
      }

      assert make_spec(cpointer_struct) == result
    end
  end

  describe "when asking for function returns for structs" do
    test "it returns a straight map" do
      result =
        quote context: Elixir do
          return_test() :: %{bar: binary(), foo: float()}
        end

      return = %Struct{
        name: "Foo",
        required: %{foo: ~t(f64)},
        optional: %{bar: ~t([]u8)}
      }

      assert make_spec(return) == result
    end

    @packed %Struct{
      name: "Foo",
      required: %{foo: ~t(f64)},
      optional: %{bar: ~t(u64)},
      packed: 16
    }

    test "it returns binary if it's packed" do
      result =
        quote context: Elixir do
          return_test() :: <<_::128>>
        end

      assert make_spec(@packed, type: :binary) == result
    end

    test "slice of packeds is what you expect" do
      result =
        quote context: Elixir do
          return_test() :: <<_::_*128>>
        end

      return = %Slice{child: @packed}

      assert make_spec(return, type: :binary) == result
    end

    test "array of packeds is what you expect" do
      result =
        quote context: Elixir do
          return_test() :: <<_::256>>
        end

      return = %Array{child: @packed, len: 2, has_sentinel?: false}

      assert make_spec(return, type: :binary) == result
    end
  end

  describe "when asking for optional returns" do
    test "it adds nil to the possible return" do
      result =
        quote context: Elixir do
          return_test() :: 0..255 | nil
        end

      assert make_spec(~t(?u8)) == result
    end
  end

  describe "when asking for resource returns" do
    test "it marks it as a reference" do
      result =
        quote context: Elixir do
          return_test() :: reference()
        end

      return = %Zig.Type.Resource{}

      assert make_spec(return) == result
    end

    test "it can know if the resource will emerge as a binary" do
      result =
        quote context: Elixir do
          return_test() :: binary()
        end

      return = %Zig.Type.Resource{}

      assert make_spec(return, type: :binary) == result
    end
  end

  describe "when there's an in-out parameter for easy_c" do
    test "it works when the length is specified" do
      result =
        quote context: Elixir do
          return_test([0..255] | binary() | nil) :: <<_::16>>
        end

      assert Function.spec(%Function{
               name: :return_test,
               arity: 0,
               params: [~t([*c]u8)],
               return: :void,
               opts: [return: [type: :default, length: 2, arg: 0]]
             }) == result
    end

    test "it works when the length is not specified" do
      result =
        quote context: Elixir do
          return_test([0..255] | binary() | nil) :: binary()
        end

      assert Function.spec(%Function{
               name: :return_test,
               arity: 0,
               params: [~t([*c]u8)],
               return: :void,
               opts: [return: [type: :default, arg: 0]]
             }) == result
    end
  end
end
