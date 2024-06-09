defmodule ZiglerTest.Types.Spec do
  @moduledoc false
  @compile :debug_info

  use Zig, otp_app: :zigler

  ~Z"""
  pub fn bool_fn(data: bool) bool { return data; }
  """
end
