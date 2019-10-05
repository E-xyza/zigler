defmodule ZigTest.MutationTest do
  use ExUnit.Case

  defmodule BinaryMutator do
    use Zigler, app: :zigler

    ~Z"""
    @nif("mutate")
    fn mutate(val: []u8) c_int {
      val[0] = 109;
      return 0;
    }
    """
  end

  def check(binary), do: binary == "mello"

  test "we can mutate binaries" do
    my_binary = "hello"
    BinaryMutator.mutate(my_binary)

    # for some reason, you must pass the reference to another
    # function in order to be able to access its mutated value
    assert check(my_binary)
  end
end
