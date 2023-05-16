defmodule ZiglerTest.Concurrency.ThreadedYieldingVeryManualTest do
  # this is the absolute minimal implementation of a threaded nif that
  # correctly yields.  It doesn't use zigler's resource wrapping, but it
  # does correctly assign internal environments, does correctly join
  # the thread, and does clean up the thread environment.

  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, cleanup: false, callbacks: [on_load: :on_load], ignore: :on_load

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  const e = @import("erl_nif");

  var resource_type: *e.ErlNifResourceType = undefined;

  fn dtor(env: beam.env, res_ptr: ?*anyopaque) callconv(.C) void {
    _ = env;
    var rres_ptr: ?*anyopaque = undefined;
    const res = @ptrCast(*Resource, @alignCast(@alignOf(Resource), res_ptr.?));
    defer e.enif_free_env(res.env);
    _ = e.enif_thread_join(res.tid, &rres_ptr);
  }

  pub fn on_load(env: beam.env, _: [*c]?*anyopaque, term: beam.term) void {
    _ = term;

    const init_struct = e.ErlNifResourceTypeInit{ .dtor = dtor, .stop = null, .down = null, .dyncall = null, .members = 1 };
    resource_type = e.enif_init_resource_type(env, "foobarbaz", &init_struct, e.ERL_NIF_RT_CREATE, null).?;

    // TODO: let this error out.
  }

  fn thread(info: ?*anyopaque) callconv(.C) ?*anyopaque {
    beam.context = .threaded;
    const res = @ptrCast(*Resource, @alignCast(@alignOf(Resource), info.?));
    _ = beam.send(res.env, res.pid, beam.make(res.env, .done, .{})) catch unreachable;
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

  const Resource = struct {
    env: beam.env,
    pid: beam.pid,
    tid: beam.tid,
  };

  pub fn launch(env: beam.env, pid: beam.pid) beam.term {
    const resptr = e.enif_alloc_resource(resource_type, @sizeOf(Resource));
    const resterm = e.enif_make_resource(env, resptr);
    defer e.enif_release_resource(resptr);

    const res = @ptrCast(*Resource, @alignCast(@alignOf(Resource), resptr));

    res.env = beam.alloc_env();
    res.pid = pid;

    _ = e.enif_thread_create(name_ptr(), &res.tid, thread, resptr, null);
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
