defmodule ZiglerTest.Concurrency.ThreadedMoreManualTest do
  # this is a slightly less minimal implementation of a threaded nif that
  # correctly yields.  It does use zigler's resource wrapping,
  # does correctly join the thread, and does clean up the thread environment,
  # but does not use any constructs from the beam.threading namespace.

  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  const e = @import("erl_nif");

  pub const ThreadResource = beam.Resource(*Thread, @import("root"), .{
    .Callbacks = struct {
      pub fn dtor(_: beam.env, dtor_ref: **Thread) void {
        const thread_ptr = dtor_ref.*;
        var rres_ptr: ?*anyopaque = undefined;
        _ = e.enif_thread_join(thread_ptr.tid, &rres_ptr);
        beam.free_env(thread_ptr.env);
        beam.allocator.destroy(thread_ptr);
      }
    }
  });

  const Thread = struct {
    env: beam.env,
    pid: beam.pid,
    tid: beam.tid,
    ref: beam.term
  };

  fn thread(info: ?*anyopaque) callconv(.C) ?*anyopaque {
    beam.context = .threaded;
    const thr = @ptrCast(*Thread, @alignCast(@alignOf(Thread), info.?));
    _ = beam.send(thr.env, thr.pid, beam.make(thr.env, .done, .{})) catch unreachable;
    return null;
  }

  const namename = "mythread";

  fn name_ptr() [*c]u8 {
      // this needs to be done like this because enif_thread_create is
      // not const-correct.  In the future, we should actually fix this
      // by giving each thread a dynamic name, so that `name` can have
      // debug information attached.
      return @intToPtr([*c]u8, @ptrToInt(&namename));
  }

  pub fn launch(env: beam.env, pid: beam.pid) !beam.term {
    const threadptr = try beam.allocator.create(Thread);
    errdefer beam.allocator.destroy(threadptr);

    const resource = ThreadResource.create(threadptr, .{}) catch unreachable;
    const res_term = beam.make(env, resource, .{});

    threadptr.env = beam.alloc_env();
    threadptr.pid = pid;
    threadptr.ref = beam.copy(threadptr.env, res_term);

    _ = e.enif_thread_create(name_ptr(), &threadptr.tid, thread, threadptr, null);
    return res_term;
  }
  """

  test "threaded function" do
    this = self()

    spawn(fn ->
      launch(this)
    end)

    assert_receive :done, 500
  end
end
