defmodule ZiglerTest.Concurrency.ThreadedYieldingManualTest do
  # recapitulates the threading process, except manually.

  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");

  const Thread = beam.Thread(@TypeOf(thread));
  pub const ThreadResource = beam.Resource(Thread, @import("root"), .{
    .Callbacks = beam.threads.ThreadCallbacks(Thread)
  });

  //fn dtor(env: beam.env, res_ptr: ?*anyopaque) callconv(.C) void {
  //  _ = env;
  //  std.debug.print("destroyed\n", .{});
  //  var rres_ptr: ?*anyopaque = undefined;
  //  const res = @ptrCast(*Resource, @alignCast(@alignOf(Resource), res_ptr.?));
  //  defer e.enif_free_env(res.env);
  //  _ = e.enif_thread_join(res.tid, &rres_ptr);
  //}

  fn thread(info: beam.pid) u32 {
    beam.context = .threaded;
    const res = @ptrCast(*Resource, @alignCast(@alignOf(Resource), info.?));
    std.debug.print("alive? {}\n", .{e.enif_is_process_alive(res.env, &res.pid)});
    const result = e.enif_send(null, &res.pid, res.env, beam.make(res.env, .done, .{}).v);
    _ = beam.send(res.env, res.pid, beam.make(res.env, .done, .{})) catch unreachable;
    return 47;
  }

  pub fn launch(env: beam.env, pid: beam.pid) beam.term {
    //const resptr = e.enif_alloc_resource(resource_type, @sizeOf(Resource));
    //const resterm = e.enif_make_resource(env, resptr);
    //defer e.enif_release_resource(resptr);
//
    //const res = @ptrCast(*Resource, @alignCast(@alignOf(Resource), resptr));
//
    //res.env = beam.alloc_env();
    //res.pid = pid;
    const resource_term = ThreadResource.create(res, .{});
    res.ref = beam.copy(res.env, resource_term)

    return beam.make(env, resource_term, .{});
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
