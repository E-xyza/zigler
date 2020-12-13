defmodule ZiglerTest.OkErrorTupleTest do
  # tests if the ok/error tuple constructors work

  use ExUnit.Case, async: true
  use Zigler

  @moduletag :okerror

  ~Z"""
  /// nif: ok_int/1
  fn ok_int(env: beam.env, val: i64) beam.term {
    return beam.make_ok_tuple(i64, env, val);
  }

  /// nif: ok_term/1
  fn ok_term(env: beam.env, val: beam.term) beam.term {
    return beam.make_ok_term(env, val);
  }

  /// nif: ok_atom/1
  fn ok_atom(env: beam.env, str: []u8) beam.term {
    return beam.make_ok_atom(env, str);
  }

  /// nif: ok_string/1
  fn ok_string(env: beam.env, str: []u8) beam.term {
    return beam.make_ok_binary(env, str);
  }

  /// nif: error_int/1
  fn error_int(env: beam.env, val: i64) beam.term {
    return beam.make_error_tuple(i64, env, val);
  }

  /// nif: error_term/1
  fn error_term(env: beam.env, val: beam.term) beam.term {
    return beam.make_error_term(env, val);
  }

  /// nif: error_atom/1
  fn error_atom(env: beam.env, str: []u8) beam.term {
    return beam.make_error_atom(env, str);
  }

  /// nif: error_string/1
  fn error_string(env: beam.env, str: []u8) beam.term {
    return beam.make_error_binary(env, str);
  }
  """

  describe "making special tuples works" do
    test "for a numeric ok tuple" do
      assert {:ok, 47} == ok_int(47)
    end

    test "for an atom ok tuple" do
      assert {:ok, :foo} == ok_atom("foo")
    end

    test "for an string ok tuple" do
      assert {:ok, "foo"} == ok_string("foo")
    end

    test "for generic ok terms" do
      assert {:ok, {1, 2}} == ok_term({1, 2})
    end

    test "for a numeric error tuple" do
      assert {:error, 47} == error_int(47)
    end

    test "for an atom error tuple" do
      assert {:error, :foo} == error_atom("foo")
    end

    test "for a string error tuple" do
      assert {:error, "foo"} == error_string("foo")
    end

    test "for generic error terms" do
      assert {:error, {1, 2}} == error_term({1, 2})
    end
  end
end
