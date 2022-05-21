defmodule ZiglerTest.Include.ZiglerMultiIncludeTest do
  #
  # tests to see if c_includes can be used with multiple .h files
  #

  use ExUnit.Case, async: true

  @moduletag :integration

  use Zig, c_includes: [c: ["fortyseven.h", "five.h"]]

  ~Z"""
  /// nif: fortytwo/0
  fn fortytwo() c_int {
    return c.FORTYSEVEN - c.FIVE;
  }
  """

  test "multiple include works from zigler preamble" do
    assert 42 == fortytwo()
  end
end
