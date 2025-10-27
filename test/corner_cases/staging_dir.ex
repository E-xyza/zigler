mod =
  "ZIGLER_STAGING_ROOT"
  |> System.fetch_env!()
  |> then(fn path ->
    # On Windows, remove drive letter (e.g., "C:\") and convert backslashes
    path
    |> String.replace(~r/^[A-Za-z]:/, "")
    |> String.replace("\\", "/")
    |> String.trim_leading("/")
  end)
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
