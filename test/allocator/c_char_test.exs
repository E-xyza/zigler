defmodule ZiglerTest.Allocator.CCharTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  pub fn foo() ![]c_char {
      const slice = try beam.allocator.alloc(c_char, 16);
      for (slice) |*c| {
          c.* = 'a';
      }
      return slice;
  }
  """

  test "allocating c_char" do
    assert ~C[aaaaaaaaaaaaaaaa] = foo()
  end
end
