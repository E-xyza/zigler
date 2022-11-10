defmodule ZiglerTest.Sema.SynchronousTest do
  use ExUnit.Case, async: true

  @moduletag :skip

  use Zig,
    compile: false

  ~Z"""
  fn synchronous() u8 { return 47; }
  """

  test "synchronous function is identified" do
    assert [%{name: :synchronous}] = @nifs
  end
end
