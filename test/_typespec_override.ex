defmodule ZiglerTest.TypespecOverride do
  @moduledoc false
  @compile :debug_info

  use Zig, otp_app: :zigler, nifs: [do_something: [specs: false]]

  @spec do_something(integer) :: integer

  ~Z"""
  const beam = @import("beam");
  pub fn do_something(term: beam.term) beam.term {
      const value = beam.get(i32, term, .{}) catch unreachable;
      return beam.make(value + 47, .{});
  }
  """
end
