defmodule ZiglerTest.Types.EmptyTest do
  # this module tests that it's possible to have degenerate functions
  # with either no parameters or void returns.

  use ExUnit.Case, async: true
  use Zigler, app: :zigler

  ~Z"""
  /// nif: zero_arity/0
  fn zero_arity() i64 {
    return 47;
  }

  /// nif: zero_arity_with_env/0
  fn zero_arity_with_env(env: beam.env) i64 {
    return 47;
  }
  """

  describe "zero arity zig functions" do
    test "work" do
      assert 47 == zero_arity()
      assert 47 == zero_arity_with_env()
    end
  end

end
