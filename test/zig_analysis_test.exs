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

  @string_function """
  const fmt = @import("std").fmt;

  @nif("concat")
  fn concat(left: [*c]u8, right: [*c]u8) [*c]u8 {
    var all_together: [100]u8 = undefined;
    const all_together_slice = all_together[0..];
    const hello_world = try fmt.bufPrint(all_together_slice, "{} {}", left, right);
    return hello_world;
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

    test "a function with strings and a preamble is correctly parsed" do
      assert Zig.tokens(@string_function) ==
        ["const", "fmt", "=", "@", "import", ["\"", "std", "\""], ".fmt", ";", "@", "nif", ["\"", "concat", "\""], "fn", "concat", ["left", ":", "[", "*", "c", "]", "u8", ",", "right", ":", "[", "*", "c", "]", "u8"], "[", "*", "c", "]", "u8", :block]
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
    assert Zig.code_spec(@string_function) == [concat: {[:"[*c]u8", :"[*c]u8"], :"[*c]u8"}]
  end

end
