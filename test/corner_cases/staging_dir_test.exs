defmodule ZiglerTest.CornerCases.StagingDirTest do
  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler, staging_directory: "./staging"

  test "specified staging dir works"
end