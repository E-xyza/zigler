defmodule ZiglerTest.CornerCases.DirectFileTest do
  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler, zig_code_path: "direct_file_test.zig"

  test "directly using file works" do
    assert 48 = add_one(47)
  end
end
