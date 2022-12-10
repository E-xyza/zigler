defmodule ZiglerTest.LeakCheckTest do
  use ExUnit.Case

  use Zig,
    otp_app: :zigler,
    nifs: [
      {:leaky_string, args: [[cleanup: false]], leak_check: true},
      {:unchecked_leak, args: [[cleanup: false]], alias: :leaky_string},
      {:no_leaky_string, args: [[cleanup: false]], leak_check: true},
      {:with_cleanup, alias: :leaky_string, leak_check: true}
    ]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  pub fn leaky_string(string: []u8) void {
    _ = string;
  }

  pub fn no_leaky_string(string: []u8) void {
    beam.allocator.free(string);
  }
  """

  describe "a leak is detected" do
    test "when you don't cleanup" do
      IO.puts("""
      ====================================================
      the following leak message is expected:
      ----------------------------------------------------
      """)

      assert_raise RuntimeError, "memory leak detected in function :leaky_string", fn ->
        leaky_string("some string")
      end

      IO.puts("====================================================")
    end
  end

  describe "tests might not report leaks" do
    test "when you actually free from inside" do
      assert :ok == no_leaky_string("some string")
    end

    test "when you don't leak check" do
      assert :ok == unchecked_leak("some string")
    end

    test "when we do perform a cleanup operation" do
      assert :ok == with_cleanup("some string")
    end
  end
end
