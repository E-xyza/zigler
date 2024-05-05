defmodule ZiglerTest.Concurrency.ThreadedManualTest do
  # recapitulates the threading process, except manually.

  use ZiglerTest.IntegrationCase, async: true

  @moduletag :threaded
  test "restore"

  # use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]
  #
  # ~Z"""
  # const beam = @import("beam");
  # const e = @import("erl_nif");
  #
  # const Thread = beam.Thread(add_47);
  # pub const ThreadResource = beam.Resource(*Thread, @import("root"), .{
  #  .Callbacks = beam.ThreadedCallbacks(Thread)
  # });
  #
  # fn add_47(x: u32) u32 {
  #  return x + 47;
  # }
  #
  # pub fn launch(x: beam.term) !beam.term {
  #  var args = [_]e.ErlNifTerm{x.v};
  #  // note that .env just needs to be there and have the right type, so we use beam.context.env.
  #  return Thread.launch(ThreadResource, 1, &args, .{.{.env = beam.context.env}});
  # }
  #
  # pub fn join(rsrc: ThreadResource) u32 {
  #  const thread = ThreadResource.unpack(rsrc);
  #  return thread.join() catch unreachable;
  # }
  # """
  #
  #  test "threaded function" do
  #    assert ref = launch(100)
  #    assert_receive {:done, ^ref}
  #    assert 147 = join(ref)
  #  end
end
