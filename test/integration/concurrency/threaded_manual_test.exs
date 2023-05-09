defmodule ZiglerTest.Concurrency.ThreadedManualTest do
  # recapitulates the threading process, except manually.

  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]

  ~Z"""
  const beam = @import("beam");

  const Thread = beam.Thread(@TypeOf(myfun));
  pub const ThreadResource = beam.Resource(Thread, @import("root"), .{});

  fn myfun(x: u32) u32 {
    return x + 47;
  }

  pub fn launch(env: beam.env, x: u32) beam.term {
    const thread = beam.allocator.create(Thread) catch unreachable;
    thread.* = Thread.prep(myfun, x, .{}) catch unreachable;
    return thread.start(env, ThreadResource) catch unreachable;
  }

  pub fn join(rsrc: ThreadResource) u32 {
    const thread = ThreadResource.unpack(rsrc);
    return thread.join() catch unreachable;
  }
  """

  test "threaded function" do
    assert ref = launch(100)
    assert_receive {:done, ^ref}
    assert 147 = join(ref)
  end
end
