defmodule ZiglerTest.Behaviour do
  @callback default() :: :ok
  @callback marked() :: :ok
end

defmodule ZiglerTest.MarkAsImpl do
  @behaviour ZiglerTest.Behaviour
  use Zig, otp_app: :zigler, nifs: [marked: [impl: true]]

  @impl true
  def default, do: :ok

  ~Z"""
  const beam = @import("beam");

  pub fn marked() void {}
  """
end
