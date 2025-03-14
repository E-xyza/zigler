defmodule ZiglerTest.Concurrency.ThreadedManualYieldingTest do
  # this is a semi manual implementation of a threaded nif.  In order to do
  # what it does, it defers all of what it does to the beam.Thread namespace.
  # this includes the thread object, locking primitives, state management, etc.
  # it does, however, create and manage the Thread resource by itself.

  use ZiglerTest.IntegrationCase, async: true

  @moduletag :threaded
  @moduletag :skip

#  use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]
#
#  ~Z"""
#  const beam = @import("beam");
#  const std = @import("std");
#  const e = @import("erl_nif");
#
#  const Thread = beam.Thread(thread);
#  pub const ThreadResource = beam.Resource(*Thread, @import("root"), .{ .Callbacks = beam.ThreadedCallbacks(Thread) });
#
#  fn thread(pid: beam.pid) void {
#      defer {
#          beam.send(pid, .done, .{}) catch {};
#      }
#
#      while (true) {
#          _ = beam.yield() catch {
#              return;
#          };
#      }
#  }
#
#  pub fn launch(pid_term: beam.term) !beam.term {
#      var args = [_]e.ErlNifTerm{pid_term.v};
#      return Thread.launch(ThreadResource, 1, &args, .{.{}});
#  }
#  """
#
#  test "threaded yield due to garbage collection" do
#    this = self()
#
#    spawn(fn ->
#      launch(this)
#    end)
#
#    assert_receive :done
#  end
end
