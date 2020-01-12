defmodule ZiglerTest.AsyncTest do
  use ExUnit.Case

  use Zigler, app: :zigler

  ~Z"""

  fn task_function() i64 {
    return 47;
  }

  /// nif: async_tester/0
  fn async_tester() i64 {
    return task_function();
  }
  """

  # STAGE 0: (should definitely work), can we call out to a function?

  @tag :one
  test "we can trigger the function" do
    assert 47 == async_tester()
  end


end
