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

  @erlnifenv_function """
  @nif("compare")
  fn compare(env: ?*e.ErlNifEnv, val1: c_int, val2: c_int) e.ErlNifTerm {
    var result: [*]const u8 = c"eq";

    if (val1 > val2) {
        result = c"gt";
    } else if (val1 < val2) {
        result = c"lt";
    }

    return e.enif_make_atom(env, result);
  }
  """

  describe "when passed to Zig.tokens/1" do
    test "a simple function is correctly parsed" do
      assert Zig.tokens(@simple_function) ==
        ["@", "nif", ["\"", "compare", "\""], "fn", "compare", ["val1", ":", "c_int", ",", "val2", ":", "c_int"], "c_int", :block]
    end

    test "a function with erlnifenv is correctly parsed" do
      assert Zig.tokens(@erlnifenv_function) ==
        ["@", "nif", ["\"", "compare", "\""], "fn", "compare", ["env", ":", "?", "*", "e.ErlNifEnv", ",", "val1", ":", "c_int", ",",  "val2", ":", "c_int"], "e.ErlNifTerm", :block]
    end
  end

  test "terms lists get turned into correct types" do
    assert Zig.params(["val1", ":", "c_int", ",", "val2", ":", "c_int"]) == [:c_int, :c_int]
    assert Zig.params(["env", ":", "?", "*", "e.ErlNifEnv", ",", "val1", ":", "c_int", ",",  "val2", ":", "c_int"])
      == [:"?*e.ErlNifEnv", :c_int, :c_int]
  end

  test "can identify the export, header and result for a simple function" do
    assert Zig.code_spec(@simple_function) == [compare: {[:c_int, :c_int], :c_int}]
    assert Zig.code_spec(@erlnifenv_function) == [compare: {[:"?*e.ErlNifEnv", :c_int, :c_int], :"e.ErlNifTerm"}]
  end

end
