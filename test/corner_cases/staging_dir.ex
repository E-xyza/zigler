mod =
  "ZIGLER_STAGING_ROOT"
  |> System.fetch_env!()
  |> String.trim_leading("/")
  |> Macro.camelize()
  |> then(&Module.concat(Zigler.StagingDir, &1))

defmodule mod do
  @moduledoc false

  use Zig, otp_app: :zigler

  ~Z"""
  pub fn add_one(number: u32) u32 {
      return number + 1;
  }
  """
end
