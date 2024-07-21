defmodule ZiglerTest.ReturnspecOverride do
  @moduledoc false
  @compile :debug_info

  use Zig, otp_app: :zigler, nifs: [do_something: [return: [spec: {:ok, integer} | :error]]]

  ~Z"""
  const beam = @import("beam");
  pub fn do_something(number: u32) beam.term {
      if (number == 42) return beam.make(.@"error", .{});
      return beam.make(.{ .ok, number }, .{});
  }
  """
end
