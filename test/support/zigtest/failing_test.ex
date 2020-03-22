defmodule ZiglerTest.ZigTest.FailingTest do

  @moduledoc false

  use Zigler

  ~Z"""
  const assert = beam.assert;

  /// nif: one/0
  fn one() i64 {
    return 1;
  }

  test "a lie" {
    assert(one() == 2);
  }

  test "a multiline lie" {
    assert(
      one() == 2
    );
  }

  test "a truth and a lie" {
    assert(1 == 1);
    assert(
      one() == 2
    );
  }
  """
end
