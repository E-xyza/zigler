#defmodule ZiglerTest.OverrideTypespec do
#  use Zig, otp_app: :zigler, nifs: [do_something: [spec: (integer -> integer)]]
#
#  ~Z"""
#  const beam = @import("beam");
#
#  pub fn do_something(env: beam.env, term: beam.term) beam.term {
#      const value = beam.get(i32, env, term, .{}) catch unreachable;
#      return beam.make(env, value + 47, .{});
#  }
#  """
#end
