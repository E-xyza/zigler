defmodule ZiglerTest.LeakCheckTest do
  use ZiglerTest.IntegrationCase

  require Logger

  use Zig,
    otp_app: :zigler,
    nifs: [
      leaky_string: [:leak_check, params: %{0 => :noclean}],
      unchecked_leak: [params: %{0 => :noclean}, alias: :leaky_string],
      with_cleanup: [:leak_check, alias: :leaky_string],
      no_leaky_string: [:leak_check, params: %{0 => [cleanup: false]}]
    ]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  pub fn leaky_string(string: []u8) usize {
      return string.len;
  }

  pub fn no_leaky_string(string: []u8) usize {
      defer beam.context.allocator.free(string);
      return string.len;
  }
  """

  describe "a leak is detected" do
    @tag :erroring
    test "when you don't cleanup" do
      Logger.warning("====== the following leak message is expected: =========== START")
      Process.sleep(200)

      assert_raise RuntimeError,
                   "memory leak detected in function `ZiglerTest.LeakCheckTest.leaky_string/1`",
                   fn ->
                     leaky_string("some string")
                   end

      Logger.warning("=========================================================== END")
    end
  end

  describe "tests might not report leaks" do
    test "when you actually free from inside" do
      assert 11 == no_leaky_string("some string")
    end

    test "when you don't leak check" do
      assert 11 == unchecked_leak("some string")
    end

    test "when we do perform a cleanup operation" do
      assert 11 == with_cleanup("some string")
    end
  end
end
