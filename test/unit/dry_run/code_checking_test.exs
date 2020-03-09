defmodule ZiglerTest.DryRun.CodeCheckingTest do
  # checks which make sure our code looks good.

  use ExUnit.Case, async: true
  use Zigler, dry_run: true

  ~Z"""
  /// nif: test1/0
  fn test1() i32 {
    return 47;
  }
  """

  ~Z"""
  /// nif: test2/0
  fn test2() i32 {
    return 42;
  }


  """

  test "two sigil_Z statements have the proper amount of spacing" do
    [zigler] = __MODULE__.__info__(:attributes)[:zigler]

    assert IO.iodata_to_binary(zigler.code) =~ """
    /// nif: test1/0
    fn test1() i32 {
      return 47;
    }

    /// nif: test2/0
    fn test2() i32 {
      return 42;
    }
    """
  end

  ~Z"""



  /// nif: test3/0
  fn test3() i32 {
    return 47;
  }
  """

  test "extra lines are trimmed" do
    [zigler] = __MODULE__.__info__(:attributes)[:zigler]

    assert IO.iodata_to_binary(zigler.code) =~  """
      return 42;
    }

    /// nif: test3/0
    fn test3() i32 {
    """
  end
end
