defmodule ZiglerTest.Sema.SynchronousTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, compile: false

  ~Z"""
  pub fn synchronous() u8 { return 47; }
  """

  test "synchronous function is identified" do
    assert [%{name: :synchronous}] = @nifs
  end
end
