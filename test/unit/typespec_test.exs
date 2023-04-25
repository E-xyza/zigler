defmodule ZiglerTest.Unit.TypespecTest do
  use ExUnit.Case, async: true

  @moduletag :skip
  @moduletag :typespec

  alias Zig.Typespec

  describe "when asking for a typespec retval" do
    test "a void function gives a sane result" do
      result =
        quote context: Elixir do
          @spec egress() :: :ok
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "void"}) == result
    end

    ###########################################################################
    ## INTS

    test "a u8-returning function gives appropriate bounds" do
      result =
        quote context: Elixir do
          @spec egress() :: 0..255
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "u8"}) == result
    end

    test "a u16-returning function gives appropriate bounds" do
      result =
        quote context: Elixir do
          @spec egress() :: 0..0xFFFF
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "u16"}) == result
    end

    test "a u32-returning function gives appropriate bounds" do
      result =
        quote context: Elixir do
          @spec egress() :: 0..0xFFFF_FFFF
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "u32"}) == result
    end

    test "a u64-returning function gives non_neg_integer" do
      result =
        quote context: Elixir do
          @spec egress() :: non_neg_integer
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "u64"}) == result
    end

    test "an i32-returning function gives appropriate bounds" do
      result =
        quote context: Elixir do
          @spec egress() :: -2_147_483_648..2_147_483_647
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "i32"}) == result
    end

    test "an i64-returning function gives integer" do
      result =
        quote context: Elixir do
          @spec egress() :: integer
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "i64"}) == result
    end

    test "a c_uint-returning function gives non_neg_integer" do
      result =
        quote context: Elixir do
          @spec egress() :: non_neg_integer
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "c_uint"}) ==
               result
    end

    test "a c_int-returning function gives integer" do
      result =
        quote context: Elixir do
          @spec egress() :: integer
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "c_int"}) == result
    end

    test "a c_ulong-returning function gives non_neg_integer" do
      result =
        quote context: Elixir do
          @spec egress() :: non_neg_integer
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "c_ulong"}) ==
               result
    end

    test "a c_long-returning function gives integer" do
      result =
        quote context: Elixir do
          @spec egress() :: integer
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "c_long"}) ==
               result
    end

    test "a usize-returning function gives non_neg_integer" do
      result =
        quote context: Elixir do
          @spec egress() :: non_neg_integer
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "usize"}) == result
    end

    test "an isize-returning function gives integer" do
      result =
        quote context: Elixir do
          @spec egress() :: integer
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "isize"}) == result
    end

    ###########################################################################
    ## FLOATS

    test "an f16-returning function gives float" do
      result =
        quote context: Elixir do
          @spec egress() :: float
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "f16"}) == result
    end

    test "an f32-returning function gives float" do
      result =
        quote context: Elixir do
          @spec egress() :: float
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "f32"}) == result
    end

    test "an f64-returning function gives float" do
      result =
        quote context: Elixir do
          @spec egress() :: float
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "f64"}) == result
    end

    ###########################################################################
    ## BOOL

    test "a bool returning function is boolean" do
      result =
        quote context: Elixir do
          @spec egress() :: boolean
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "bool"}) == result
    end

    ###########################################################################
    ## BEAM

    test "a beam.term returning function is term" do
      result =
        quote context: Elixir do
          @spec egress() :: term
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "beam.term"}) ==
               result
    end

    test "a e.ErlNifTerm returning function is term" do
      result =
        quote context: Elixir do
          @spec egress() :: term
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "e.ErlNifTerm"}) ==
               result
    end

    test "a beam.pid returning function is pid" do
      result =
        quote context: Elixir do
          @spec egress() :: pid
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "beam.pid"}) ==
               result
    end

    test "a e.ErlNifPid returning function is pid" do
      result =
        quote context: Elixir do
          @spec egress() :: pid
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "e.ErlNifPid"}) ==
               result
    end

    test "a beam.atom returning function is atom" do
      result =
        quote context: Elixir do
          @spec egress() :: atom
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "beam.atom"}) ==
               result
    end

    ###########################################################################
    ## COLLECTIONS

    test "a u8-slice returning function is special and turns to binary" do
      result =
        quote context: Elixir do
          @spec egress() :: binary
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "[]u8"}) == result
    end

    test "a int-slice returning function is list of integer" do
      result =
        quote context: Elixir do
          @spec egress() :: [integer]
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "[]i64"}) == result
    end

    test "a float-slice returning function is list of integer" do
      result =
        quote context: Elixir do
          @spec egress() :: [float]
        end

      assert Typespec.from_nif(%{name: :egress, arity: 0, args: [], retval: "[]f64"}) == result
    end
  end

  describe "when asking for a typed input" do
    test "?*e.ErlNifEnv is ignored for zero arity" do
      result =
        quote context: Elixir do
          @spec ingress() :: :ok
        end

      assert Typespec.from_nif(%{
               name: :ingress,
               arity: 0,
               args: ["?*e.ErlNifEnv"],
               retval: "void"
             }) == result
    end

    test "beam.env is ignored for zero arity" do
      result =
        quote context: Elixir do
          @spec ingress() :: :ok
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 0, args: ["beam.env"], retval: "void"}) ==
               result
    end

    test "?*e.ErlNifEnv is ignored for one arity" do
      result =
        quote context: Elixir do
          @spec ingress(integer) :: integer
        end

      assert Typespec.from_nif(%{
               name: :ingress,
               arity: 1,
               args: ["?*e.ErlNifEnv", "i64"],
               retval: "i64"
             }) == result
    end

    test "beam.env is ignored for one arity" do
      result =
        quote context: Elixir do
          @spec ingress(integer) :: integer
        end

      assert Typespec.from_nif(%{
               name: :ingress,
               arity: 1,
               args: ["beam.env", "i64"],
               retval: "i64"
             }) == result
    end

    ###########################################################################
    ## INTS

    test "a u8-input function gives appropriate bounds" do
      result =
        quote context: Elixir do
          @spec ingress(0..255) :: 0..255
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["u8"], retval: "u8"}) ==
               result
    end

    test "a u16-input function gives appropriate bounds" do
      result =
        quote context: Elixir do
          @spec ingress(0..0xFFFF) :: 0..0xFFFF
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["u16"], retval: "u16"}) ==
               result
    end

    test "a u32-input function gives appropriate bounds" do
      result =
        quote context: Elixir do
          @spec ingress(0..0xFFFF_FFFF) :: 0..0xFFFF_FFFF
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["u32"], retval: "u32"}) ==
               result
    end

    test "a u64-input function gives non_neg_integer" do
      result =
        quote context: Elixir do
          @spec ingress(non_neg_integer) :: non_neg_integer
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["u64"], retval: "u64"}) ==
               result
    end

    test "an i32-input function gives appropriate bounds" do
      result =
        quote context: Elixir do
          @spec ingress(-2_147_483_648..2_147_483_647) :: -2_147_483_648..2_147_483_647
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["i32"], retval: "i32"}) ==
               result
    end

    test "an i64-input function gives integer" do
      result =
        quote context: Elixir do
          @spec ingress(integer) :: integer
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["i64"], retval: "i64"}) ==
               result
    end

    test "a c_uint-input function gives non_neg_integer" do
      result =
        quote context: Elixir do
          @spec ingress(non_neg_integer) :: non_neg_integer
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["c_uint"], retval: "c_uint"}) ==
               result
    end

    test "a c_int-input function gives integer" do
      result =
        quote context: Elixir do
          @spec ingress(integer) :: integer
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["c_int"], retval: "c_int"}) ==
               result
    end

    test "a c_ulong-input function gives non_neg_integer" do
      result =
        quote context: Elixir do
          @spec ingress(non_neg_integer) :: non_neg_integer
        end

      assert Typespec.from_nif(%{
               name: :ingress,
               arity: 1,
               args: ["c_ulong"],
               retval: "c_ulong"
             }) == result
    end

    test "a c_long-input function gives integer" do
      result =
        quote context: Elixir do
          @spec ingress(integer) :: integer
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["c_long"], retval: "c_long"}) ==
               result
    end

    test "a usize-input function gives non_neg_integer" do
      result =
        quote context: Elixir do
          @spec ingress(non_neg_integer) :: non_neg_integer
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["usize"], retval: "usize"}) ==
               result
    end

    test "an isize-input function gives integer" do
      result =
        quote context: Elixir do
          @spec ingress(integer) :: integer
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["isize"], retval: "isize"}) ==
               result
    end

    ###########################################################################
    ## FLOATS

    test "an f16-input function gives float" do
      result =
        quote context: Elixir do
          @spec ingress(float) :: float
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["f16"], retval: "f16"}) ==
               result
    end

    test "an f32-input function gives float" do
      result =
        quote context: Elixir do
          @spec ingress(float) :: float
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["f32"], retval: "f32"}) ==
               result
    end

    test "an f64-input function gives float" do
      result =
        quote context: Elixir do
          @spec ingress(float) :: float
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["f64"], retval: "f64"}) ==
               result
    end

    ###########################################################################
    ## BOOL

    test "a bool input function is boolean" do
      result =
        quote context: Elixir do
          @spec ingress(boolean) :: boolean
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["bool"], retval: "bool"}) ==
               result
    end

    ###########################################################################
    ## BEAM

    test "a beam.term input function is term" do
      result =
        quote context: Elixir do
          @spec ingress(term) :: term
        end

      assert Typespec.from_nif(%{
               name: :ingress,
               arity: 1,
               args: ["beam.term"],
               retval: "beam.term"
             }) == result
    end

    test "a e.ErlNifTerm input function is term" do
      result =
        quote context: Elixir do
          @spec ingress(term) :: term
        end

      assert Typespec.from_nif(%{
               name: :ingress,
               arity: 1,
               args: ["e.ErlNifTerm"],
               retval: "e.ErlNifTerm"
             }) == result
    end

    test "a beam.pid input function is pid" do
      result =
        quote context: Elixir do
          @spec ingress(pid) :: pid
        end

      assert Typespec.from_nif(%{
               name: :ingress,
               arity: 1,
               args: ["beam.pid"],
               retval: "beam.pid"
             }) == result
    end

    test "a e.ErlNifPid input function is pid" do
      result =
        quote context: Elixir do
          @spec ingress(pid) :: pid
        end

      assert Typespec.from_nif(%{
               name: :ingress,
               arity: 1,
               args: ["e.ErlNifPid"],
               retval: "e.ErlNifPid"
             }) == result
    end

    test "a beam.atom input function is atom" do
      result =
        quote context: Elixir do
          @spec ingress(atom) :: atom
        end

      assert Typespec.from_nif(%{
               name: :ingress,
               arity: 1,
               args: ["beam.atom"],
               retval: "beam.atom"
             }) == result
    end

    ###########################################################################
    ## COLLECTIONS

    test "a u8-slice returning function is special and turns to binary" do
      result =
        quote context: Elixir do
          @spec ingress(binary) :: binary
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["[]u8"], retval: "[]u8"}) ==
               result
    end

    test "a int-slice returning function is list of integer" do
      result =
        quote context: Elixir do
          @spec ingress([integer]) :: [integer]
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["[]i64"], retval: "[]i64"}) ==
               result
    end

    test "a float-slice returning function is list of integer" do
      result =
        quote context: Elixir do
          @spec ingress([float]) :: [float]
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["[]f64"], retval: "[]f64"}) ==
               result
    end

    test "multi-speccing works" do
      result =
        quote context: Elixir do
          @spec ingress(float, integer) :: float
        end

      assert Typespec.from_nif(%{name: :ingress, arity: 1, args: ["f64", "i64"], retval: "f64"}) ==
               result
    end
  end
end
