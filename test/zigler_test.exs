defmodule ZiglerTest do
  use ExUnit.Case

  defmodule SigilZ do

    use Zigler, app: :zigler

    ~Z"""
    @nif("compare")
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
end
