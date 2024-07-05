defmodule ZiglerTest.LeakCheckTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [
      leaky_string: [:leak_check, params: [[cleanup: false]]],
      unchecked_leak: [params: [[cleanup: false]], alias: :leaky_string],
      with_cleanup: [:leak_check, alias: :leaky_string],
      no_leaky_string: [:leak_check, params: [[cleanup: false]]]
    ]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  pub fn leaky_string(string: []u8) usize {
    return string.len;
  }

  pub fn no_leaky_string(string: []u8) usize {
    defer beam.allocator.free(string);
    return string.len;
  }
  """

  describe "a leak is detected" do
    @tag :erroring
    test "when you don't cleanup" do
      IO.puts(:stderr, """
      ====================================================
      the following leak message is expected:
      ----------------------------------------------------
      """)

      assert_raise RuntimeError, "memory leak detected in function `ZiglerTest.LeakCheckTest.leaky_string/1`", fn ->
        leaky_string("some string")
      end

      IO.puts(:stderr, "====================================================")
    end
  end

  describe "tests might not report leaks" do
    @tag :skip
    # this is currently segfaulting
    test "when you actually free from inside" do
      assert 11 == no_leaky_string("some string")
    end

    # @tag :fails_macos
    test "when you don't leak check" do
      assert 11 == unchecked_leak("some string")
    end

    test "when we do perform a cleanup operation" do
      assert 11 == with_cleanup("some string")
    end
  end
end
