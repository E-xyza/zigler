defmodule ZiglerTest.Types.PidTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    leak_check: true,
    otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  pub fn pid_dance(pids: []beam.pid) beam.pid {
    // send tuple {:ok, pid[0]} to the pid[1], return pid[1]
    _ = beam.send(pids[1], .{.ok, pids[0]}, .{}) catch unreachable;
    return pids[1];
  }
  """

  # TODO: make sure that the "count" thing is the type we expect.

  describe "beam parameters" do
    test "are valid" do
      spawned =
        spawn(fn ->
          receive do
            {:ok, pid} -> send(pid, :done)
          end
        end)

      assert spawned == pid_dance([self(), spawned])
      assert_receive :done, 1000
    end
  end
end
