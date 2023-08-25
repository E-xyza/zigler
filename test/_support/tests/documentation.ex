# defmodule ZiglerTest.Documentation do
#  use Zig, otp_app: :zigler, nifs: [..., no_docs: [docs: false]]
#
#  ~Z"""
#  const beam = @import("beam");
#
#  /// This is a function that does something.
#  pub fn do_something(env: beam.env, term: beam.term) beam.term {
#      const value = beam.get(i32, env, term, .{}) catch unreachable;
#      return beam.make(env, value + 47, .{});
#  }
#
#  /// This function has its docs suppressed
#  pub fn no_docs(term: beam.term) beam.term {
#      return term;
#  }
#  """
# end
