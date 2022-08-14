defmodule ZiglerTest.Include.ZiglerIncludeTest do
  #
  # tests to see if c_includes can be used to include headers as expected
  #

  use ExUnit.Case, async: true

  @moduletag :integration

  use Zig, c_includes: [c: "fortyseven.h"]

  ~Z"""
  /// nif: fortyseven/0
  fn fortyseven() c_int {
    return c.FORTYSEVEN;
  }
  """

  test "single include works from zigler preamble" do
    assert 47 == fortyseven()
  end
end

defmodule ZiglerTest.Include.ZiglerIncludeAltNameTest do
  # tries a different name

  use ExUnit.Case, async: true
  use Zig, c_includes: [namespace: "fortyseven.h"]

  ~Z"""
  /// nif: fortyseven/0
  fn fortyseven() c_int {
    return namespace.FORTYSEVEN;
  }
  """

  test "single include works from zigler preamble" do
    assert 47 == fortyseven()
  end
end
