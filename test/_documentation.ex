defmodule ZiglerTest.Documentation do
  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  /// This is a function that does something
  pub fn do_something(term: beam.term) beam.term {
      const value = beam.get(i32, term, .{}) catch unreachable;
      return beam.make(value + 47, .{});
  }
  """
end
