defmodule ZiglerTest do
  use ExUnit.Case, async: true

  defmodule SigilZ do

    use Zigler, app: :zigler

    ~Z"""
    @nif("compare");
    fn compare(val1: c_int, val2: c_int) c_int {
      var result: c_int = 0;

      if (val1 > val2) {
          result = 1;
      } else if (val1 < val2) {
          result = -1;
      }

      return result;
    }
    """
  end

  test "sigil-Z creates a public function" do
    assert 1 == SigilZ.compare(2, 1)
    assert 0 == SigilZ.compare(1, 1)
    assert -1 == SigilZ.compare(1, 2)
  end

  defmodule SigilZ2 do

    use Zigler, app: :zigler

    ~Z"""
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
  end

  test "sigil-Z2 creates a function of the correct arity" do
    assert :gt == SigilZ2.compare(2, 1)
    assert :eq == SigilZ2.compare(1, 1)
    assert :lt == SigilZ2.compare(1, 2)
  end
end
