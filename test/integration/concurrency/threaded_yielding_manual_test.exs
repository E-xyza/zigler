defmodule ZiglerTest.Concurrency.ThreadedYieldingManualTest do
  # recapitulates the threading process, except manually.

  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");

  const Thread = beam.Thread(@TypeOf(myfun));
  pub const ThreadResource = beam.Resource(*Thread, @import("root"), .{
    .Callbacks = beam.threads.ThreadCallbacks(Thread)
  });

  fn myfun(pid : beam.pid) u32 {
      const env = Thread.get_info().env;
      var loop: bool = true;
      while (loop) {
          _ = beam.yield(.{}) catch { loop = false; };
      }
      _ = beam.send(env, pid, .done) catch {};
      return 47;
  }

  pub fn launch(env: beam.env, pid: beam.pid) beam.term {
    const thread = Thread.create(myfun, pid, .{}) catch unreachable;
    return thread.start(env, ThreadResource) catch unreachable;
  }
  """

  test "threaded function" do
    this = self()

    spawn(fn ->
      launch(this)
    end)

    assert_receive :done
    Process.sleep(10)
  end
end
