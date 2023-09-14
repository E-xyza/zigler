defmodule ZiglerTest.Concurrency.ThreadedManualYieldingTest do
  # this is a semi manual implementation of a threaded nif.  In order to do
  # what it does, it defers all of what it does to the beam.threading namespace.
  # this includes the thread object, locking primitives, state management, etc.
  # it does, however, create and manage the Thread resource by itself.

  use ZiglerTest.IntegrationCase, async: true

  test "skipped"

#  @moduletag :threaded
#
#  use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]
#
#  ~Z"""
#  const beam = @import("beam");
#  const std = @import("std");
#  const e = @import("erl_nif");
#
#  const Thread = beam.Thread(thread);
#  pub const ThreadResource = beam.Resource(*Thread, @import("root"), .{
#    .Callbacks = beam.ThreadedCallbacks(Thread)
#  });
#
#  fn thread(env: beam.env, pid: beam.pid) void {
#    defer {
#      _ = beam.send(env, pid, beam.make(env, .done, .{})) catch {};
#    }
#
#    while (true) {
#      _ = beam.yield(env) catch { return; };
#    }
#  }
#
#  pub fn launch(env: beam.env, pid_term: beam.term) !beam.term {
#    var args = [_]e.ErlNifTerm{pid_term.v};
#    return Thread.launch(ThreadResource, env, 1, &args, .{.arg_opts = .{.{}}});
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
