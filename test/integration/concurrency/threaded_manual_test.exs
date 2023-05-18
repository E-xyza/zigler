defmodule ZiglerTest.Concurrency.ThreadedManualTest do
  # recapitulates the threading process, except manually.

  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]

  ~Z"""
  const beam = @import("beam");

  const Thread = beam.Thread(myfun);
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

  pub fn join(rsrc: ThreadResource) u32 {
    const thread = ThreadResource.unpack(rsrc);
    return thread.join(.{}) catch unreachable;
  }
  """

  test "threaded function" do
    assert ref = launch(100)
    assert 147 = join(ref)
  end
end
