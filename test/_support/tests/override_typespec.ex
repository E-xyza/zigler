# defmodule ZiglerTest.OverrideTypespec do
#  @moduledoc false
#  use Zig, otp_app: :zigler, nifs: [do_something: [spec: (integer -> integer)]]
#
#  ~Z"""
#  const beam = @import("beam");
#  pub fn do_something(term: beam.term) beam.term {
#      const value = beam.get(i32, term, .{}) catch unreachable;
#      return beam.make(value + 47, .{});
#  }
#  """
# end
#
