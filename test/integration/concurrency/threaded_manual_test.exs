defmodule ZiglerTest.Concurrency.ThreadedManualTest do
  # recapitulates the threading process, except manually.

  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]

  ~Z"""
  const beam = @import("beam");

  const Thread = beam.Thread(@TypeOf(myfun));
  pub const ThreadResource = beam.Resource(*Thread, @import("root"), .{
    .Callbacks = beam.threads.ThreadCallbacks(Thread)
  });

  fn myfun(x: u32) u32 {
    return x + 47;
  }

  pub fn launch(env: beam.env, x: u32) beam.term {
    const thread = Thread.create(myfun, x, .{}) catch unreachable;
    return thread.start(env, ThreadResource) catch unreachable;
  }

  pub fn join(rsrc: ThreadResource) void {
    const thread = ThreadResource.unpack(rsrc);
    _ = thread.join(.{}) catch unreachable;
  }
  """

  test "threaded function" do
    assert ref = launch(100)
    assert_receive {:done, ^ref, 147}
    assert join(ref)
  end
end
