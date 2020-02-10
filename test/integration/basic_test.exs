defmodule ZiglerTest.Integration.BasicTest do
  use ExUnit.Case, async: true

  defmodule Child do
    use Zigler
    ~Z"""
    /// nif: fortyseven/0
    fn fortyseven() i64 {
      return 47;
    }
    """
  end

  test "zero-arity function works" do
    assert 47 == Child.fortyseven()
  end

end
