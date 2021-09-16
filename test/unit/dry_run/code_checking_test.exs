defmodule ZiglerTest.DryRun.CodeCheckingTest do
  # checks which make sure our code looks good.

  use ExUnit.Case, async: true
  use Zig, dry_run: true

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
    const std = @import("std");
    const e = @import("erl_nif.zig");
    const beam = @import("beam.zig");


    // ref: test/unit/dry_run/code_checking_test.exs line: 7
    /// nif: test1/0
    fn test1() i32 {
      return 47;
    }

    // ref: test/unit/dry_run/code_checking_test.exs line: 14
    /// nif: test2/0
    fn test2() i32 {
      return 42;
    }
    """
  end
end
