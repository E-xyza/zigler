defmodule ZiglerTest.Types do
  use Zigler, dry_run: true

  @spec dummy_integer(integer) :: integer
  def dummy_integer(a), do: a

  ~Z"""
  /// nif: void_out/0
  fn void_out() void {}
  """
end
