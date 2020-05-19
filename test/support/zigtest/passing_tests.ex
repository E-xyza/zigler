defmodule ZiglerTest.ZigTest.PassingTests do
  @moduledoc false

  use Zigler

  ~Z"""
  /// nif: forty_seven/0
  fn forty_seven() i32 {
    return 47;
  }

  test "forty seven returns forty seven" {
    assert(47 == forty_seven());
  }

  test "system has access to beam test env" {
    var foo = beam.make_i32(beam.test_env, 47);
    assert(47 ==
      beam.get_i32(beam.test_env, foo) catch unreachable
    );
  }
  """
end
