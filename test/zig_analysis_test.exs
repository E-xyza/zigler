defmodule ZiglerTest.ZigAnalysisTest do
  use ExUnit.Case

  alias Zigler.Zig

  @simple_function """
  @nif("compare")
  fn compare(val1: c_int, val2: c_int) c_int {
    var result: c_int = 0;

    if (a > b) {
        result = 1;
    } else if (a < b) {
        result = -1;
    }

    return result
  }
  """

  test "can parse the function" do
    assert Zig.tokens(@simple_function) ==
      ["@", "nif", ["\"", "compare", "\""], "fn", "compare", ["val1", ":", "c_int", ",", "val2", ":", "c_int"], "c_int", :block]
  end

  test "terms lists get turned into correct types" do
    assert Zig.params(["val1", ":", "c_int", ",", "val2", ":", "c_int"]) == [:c_int, :c_int]
  end

  test "can identify the export, header and result for a simple function" do
    assert Zig.code_spec(@simple_function) == [compare: {[:c_int, :c_int], :c_int}]
  end

end
