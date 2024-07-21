defmodule ZiglerTest.Raw.Typespec do
  @compile :debug_info
  use Zig, otp_app: :zigler, nifs: [raw: [arity: 1], multi_raw: [arity: [1, 3..4]]]

  ~Z"""
  const beam = @import("beam");
  pub fn raw(_: beam.env, _: c_int, terms: [*]const beam.term) beam.term {
      return terms[0];
  }

  pub fn multi_raw(_: beam.env, _: c_int, terms: [*]const beam.term) beam.term {
      return terms[0];
  }
  """
end
