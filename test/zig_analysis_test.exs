defmodule ZiglerTest.ZigAnalysisTest do
  use ExUnit.Case, async: true

  alias Zigler.Parser
  alias Zigler.Code

  @simple_function """
  @nif("compare");
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
  @nif("compare");
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

  @nif("concat");
  fn concat(left: [*c]u8, right: [*c]u8) [*c]u8 {
    var all_together: [100]u8 = undefined;
    const all_together_slice = all_together[0..];
    const hello_world = try fmt.bufPrint(all_together_slice, "{} {}", left, right);
    return hello_world;
  }
  """

  def code_spec(txt) do
    1 |> Parser.tokenize(txt) |> Code.to_spec
  end

  test "can identify the export, header and result for a simple function" do
    assert code_spec(@simple_function) == [compare: {[:c_int, :c_int], :c_int}]
    assert code_spec(@erlnifenv_function) == [compare: {[:"?*e.ErlNifEnv", :c_int, :c_int], :"e.ErlNifTerm"}]
    assert code_spec(@string_function) == [concat: {[:cstring, :cstring], :cstring}]
  end

end
