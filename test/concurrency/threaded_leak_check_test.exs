defmodule ZiglerTest.Concurrency.ThreadedLeakCheckTest do
  use ZiglerTest.IntegrationCase, async: true

  require Logger

  @moduletag :threaded

  use Zig,
    otp_app: :zigler,
    nifs: [
      leaky_threaded: [:threaded, :leak_check, params: %{0 => :noclean}],
      unchecked_leak: [:threaded, params: %{0 => :noclean}],
      with_cleanup: [:threaded, :leak_check],
      no_leaky_threaded: [:threaded, :leak_check, params: %{0 => [cleanup: false]}]
    ]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");

  pub fn leaky_threaded(string: []u8) usize {
      return string.len;
  }

  pub fn unchecked_leak(string: []u8) usize {
      return string.len;
  }

  pub fn with_cleanup(string: []u8) usize {
      return string.len;
  }

  pub fn no_leaky_threaded(string: []u8) usize {
      defer beam.context.allocator.free(string);
      return string.len;
  }
  """

  describe "a leak is detected" do
    # leak_check is disabled on Windows - see issue #591
    @tag :erroring
    @tag :no_windows
    test "when you don't cleanup" do
      Logger.warning("====== the following leak message is expected: =========== START")
      Process.sleep(200)

      assert_raise RuntimeError,
                   "memory leak detected in threaded function `leaky_threaded`",
                   fn ->
                     leaky_threaded("some string")
                   end

      Logger.warning("=========================================================== END")
    end
  end

  describe "tests might not report leaks" do
    test "when you actually free from inside" do
      assert 11 == no_leaky_threaded("some string")
    end

    test "when you don't leak check" do
      assert 11 == unchecked_leak("some string")
    end

    test "when we do perform a cleanup operation" do
      assert 11 == with_cleanup("some string")
    end
  end
end
