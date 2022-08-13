defmodule ZiglerTest.Sema.FunctionTest do
  use ExUnit.Case, async: true

  alias Zig.Type
  alias Zig.Sema

  import Type, only: :macros

  # basic test on semantic analysis
  @basic """
  fn basic(x: u8) u8 {
    return x + 1;
  }
  """

  test "a basic function can be found" do
    assert [%Type.Function{
      name: :basic,
      arity: 1,
      params: [~t(u8)],
      return: ~t(u8),
    }] = Zig.Sema.analyze_string!(@basic)
  end
end
