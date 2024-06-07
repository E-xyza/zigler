defmodule ZiglerTest.Raw.Typespec do
  @compile :debug_info
  use Zig, otp_app: :zigler, nifs: [raw: [arity: 1]]

  ~Z"""
  const beam = @import("beam");
  pub fn raw(_: beam.env, _: c_int, terms: [*]const beam.term) beam.term {
    return terms[0];
  }
  """
end
