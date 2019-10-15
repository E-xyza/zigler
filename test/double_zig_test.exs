defmodule ZigTest.DoubleZigTest do
  use ExUnit.Case, async: true

  defmodule DoubleZig1 do

    # tests more than one zig in the same sigil Z

    use Zigler, app: :zigler

    ~Z"""
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

    @nif("reverse")
    fn reverse(env: ?*e.ErlNifEnv, val1: c_int, val2: c_int) e.ErlNifTerm {
      var result: [*]const u8 = c"eq";

      if (val1 < val2) {
          result = c"gt";
      } else if (val1 > val2) {
          result = c"lt";
      }

      return e.enif_make_atom(env, result);
    }
    """
  end

  test "two zig definitions in the same sigil" do
    assert DoubleZig1.compare(1, 2) == :lt
    assert DoubleZig1.reverse(1, 2) == :gt
  end

  defmodule DoubleZig2 do

    # tests two zigs in two sigil Z

    use Zigler, app: :zigler

    ~Z"""
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

    ~Z"""
    @nif("reverse")
    fn reverse(env: ?*e.ErlNifEnv, val1: c_int, val2: c_int) e.ErlNifTerm {
      var result: [*]const u8 = c"eq";

      if (val1 < val2) {
          result = c"gt";
      } else if (val1 > val2) {
          result = c"lt";
      }

      return e.enif_make_atom(env, result);
    }
    """
  end

  test "two zig definitions in the different sigils" do
    assert DoubleZig2.compare(1, 2) == :lt
    assert DoubleZig2.reverse(1, 2) == :gt
  end
end
