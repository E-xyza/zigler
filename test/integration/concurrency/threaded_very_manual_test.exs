defmodule ZiglerTest.Concurrency.ThreadedVeryManualTest do
  # this is the absolute minimal implementation of a threaded nif that
  # correctly yields.  It doesn't use zigler's resource wrapping, but it
  # does correctly assign internal environments, does correctly join
  # the thread, and does clean up the thread environment.

  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, cleanup: false, callbacks: [on_load: :on_load]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  const e = @import("erl_nif");

  var resource_type: *e.ErlNifResourceType = undefined;

  fn dtor(env: beam.env, res_ptr: ?*anyopaque) callconv(.C) void {
    _ = env;
    var rres_ptr: ?*anyopaque = undefined;
    const res: *Resource = @ptrCast(@alignCast(res_ptr.?));
    defer e.enif_free_env(res.env);
    _ = e.enif_thread_join(res.tid, &rres_ptr);
  }

  pub fn on_load(env: beam.env, _: [*c]?*anyopaque, term: beam.term) c_int {
    _ = term;

    const init_struct = e.ErlNifResourceTypeInit{ .dtor = dtor, .stop = null, .down = null, .dyncall = null, .members = 1 };
    resource_type = e.enif_init_resource_type(env, "threadedmanualresource", &init_struct, e.ERL_NIF_RT_CREATE, null).?;
    return 0;
  }

  fn thread(info: ?*anyopaque) callconv(.C) ?*anyopaque {
    const res: *Resource = @ptrCast(@alignCast(info.?));
    beam.context = .{
      .mode = .threaded,
      .env = res.env,
      .allocator = beam.allocator,
    };
    _ = beam.send(res.pid, beam.make(.done, .{}), .{}) catch unreachable;
    return null;
  }

  const namename = "mythread";

  const Resource = struct {
    env: beam.env,
    pid: beam.pid,
    tid: beam.tid,
  };

  pub fn launch(pid: beam.pid) beam.term {
    const resptr = e.enif_alloc_resource(resource_type, @sizeOf(Resource));
    const resterm = e.enif_make_resource(beam.get_env(), resptr);
    defer e.enif_release_resource(resptr);

    const res: *Resource = @ptrCast(@alignCast(resptr));

    res.env = beam.alloc_env();
    res.pid = pid;

    _ = e.enif_thread_create(@constCast(namename.ptr), &res.tid, thread, resptr, null);
    return .{.v = resterm};
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
