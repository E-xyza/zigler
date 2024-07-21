defmodule ZiglerTest.Concurrency.ThreadedManualTest do
  # recapitulates the threading process, except manually.

  use ZiglerTest.IntegrationCase, async: true

  @moduletag :threaded

  use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  const Thread = beam.Thread(add_47);
  pub const ThreadResource = beam.Resource(*Thread, @import("root"), .{ .Callbacks = beam.ThreadedCallbacks(Thread) });

  fn add_47(x: u32) u32 {
      return x + 47;
  }

  pub fn launch(x: beam.term) !beam.term {
      var args = [_]e.ErlNifTerm{x.v};
      const abc = try Thread.launch(ThreadResource, 1, &args, .{.{}});
      return abc;
  }

  pub fn join(rsrc: ThreadResource) u32 {
      const thread = ThreadResource.unpack(rsrc);
      return thread.join() catch unreachable;
  }
  """

  test "threaded function" do
    assert ref = launch(100)
    assert_receive {:done, ^ref}, 1000
    assert 147 = join(ref)
  end
end
