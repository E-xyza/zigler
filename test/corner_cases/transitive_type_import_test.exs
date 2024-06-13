defmodule ZiglerTest.CornerCases.TransitiveTypeImportTest do
  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler

  ~Z"""
  const types = @import("transitive_types.zig");
  const S = types.S;
  pub fn identity(value: S) S { return value; }
  """

  test "transitive types work" do
    assert %{foo: 47} = identity(%{foo: 47})
  end

end