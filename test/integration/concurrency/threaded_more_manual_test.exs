defmodule ZiglerTest.Concurrency.ThreadedMoreManualTest do
  # this is a slightly less minimal implementation of a threaded nif that
  # correctly yields.  It does use zigler's resource wrapping,
  # does correctly join the thread, and does clean up the thread environment,
  # but does not use any constructs from the beam.threading namespace.

  use ZiglerTest.IntegrationCase, async: true

  @moduletag :threaded
  @moduletag :skip

  use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  const e = @import("erl_nif");

  pub const ThreadResource = beam.Resource(*Thread, @import("root"), .{
    .Callbacks = struct {
      pub fn dtor(dtor_ref: **Thread) void {
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
    const thr: *Thread = @ptrCast(@alignCast(info.?));

    beam.context = .{
      .mode = .threaded,
      .env = thr.env,
      .allocator = beam.allocator,
    };

    _ = beam.send(thr.pid, .done, .{}) catch unreachable;
    return null;
  }

  const namename = "mythread";

  pub fn launch(pid: beam.pid) !beam.term {
    const threadptr = try beam.allocator.create(Thread);
    errdefer beam.allocator.destroy(threadptr);

    const resource = ThreadResource.create(threadptr, .{}) catch unreachable;
    const res_term = beam.make(resource, .{});

    threadptr.env = beam.alloc_env();
    threadptr.pid = pid;
    threadptr.ref = beam.copy(threadptr.env, res_term);

    _ = e.enif_thread_create(@constCast(namename), &threadptr.tid, thread, threadptr, null);
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
