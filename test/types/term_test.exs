defmodule ZiglerTest.Types.TermTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  pub fn term_test(term: beam.term) beam.term {
      return term;
  }
  """

  describe "for a generic term" do
    test "you can return a term" do
      assert 1 == term_test(1)
    end
  end
end
