defmodule ZigTest.MutationTest do
  use ExUnit.Case, async: true

  defmodule BinaryMutator do
    use Zigler, app: :zigler

    ~Z"""
    /// nif: mutate/1
    fn mutate(val: []u8) c_int {
      val[0] = 109; // set it to ascii letter 'm'

      // don't return the binary, we don't want to trigger a conversion.
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

  def self_deal(binary, 1) do
    BinaryMutator.mutate(binary)
    self_deal(binary, 0)
  end
  def self_deal(binary, 0), do: binary

  test "passing binaries across a function self-call works" do
    assert "mello" == self_deal("hello", 1)
  end
end
