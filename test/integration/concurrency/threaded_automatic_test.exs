defmodule ZiglerTest.Concurrency.ThreadedAutomaticTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :threaded

  use Zig, otp_app: :zigler, nifs: [threaded: [:threaded]]

  ~Z"""
  const std = @import("std");
  const beam = @import("beam");
  const e = @import("erl_nif");
  pub fn threaded(env: beam.env, should_quit: bool, resp: beam.pid) void {
    var i: u32 = 0;
    while (true) : (i += 1) {
      if (i > 5000 and should_quit) {
        break;
      }

      _ = beam.yield() catch {
        _ = beam.send(resp, .killed, .{}) catch unreachable;
        break;
      };
    }
  }
  """

  test "threaded function" do
    assert :ok = threaded(true, self())
    refute_receive :killed
  end

  test "threaded function gc's" do
    this = self()
    pid = spawn(fn -> threaded(false, this) end)
    :erlang.garbage_collect(pid)
    Process.exit(pid, :kill)
    assert_receive :killed, 500
  end
end
