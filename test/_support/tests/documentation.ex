defmodule ZiglerTest.Documentation do
  @moduledoc false
  use Zig, otp_app: :zigler, nifs: [..., no_docs: [docs: false]]

  ~Z"""
  const beam = @import("beam");

  /// This is a function that does something.
  pub fn do_something(term: beam.term) beam.term {
      const value = beam.get(i32, term, .{}) catch unreachable;
      return beam.make(value + 47, .{});
  }

  /// This function has its docs suppressed
  pub fn no_docs(term: beam.term) beam.term {
      return term;
  }
  """
end
