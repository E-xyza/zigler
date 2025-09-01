defmodule ZiglerTest.CornerCases.BuildZigOverrideTest do
  # uses the "build.zig" from `direct_file_test`
  use ExUnit.Case, async: true

  # note that the import statement here is missing the module import, which is
  # completely taken care of by the build_files_dir.  This is not necessarily
  # the best use case for build_files_dir, but is good at exercising the
  # relevant functionality.
  
  use Zig, otp_app: :zigler, build_files_dir: "build_files"

  ~Z"""
  pub const add_one = @import("module").add_one;
  """

  test "directly using file works" do
    assert 48 = add_one(47)
  end
end
