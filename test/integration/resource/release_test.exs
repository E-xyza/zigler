defmodule ZiglerTest.Resource.ReleaseTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, resources: [:PidResource]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  const Resource = beam.Resource;

  pub const PidResource = Resource(beam.pid, @import("root"), .{.Callbacks = PidResourceCallbacks});

  pub const PidResourceCallbacks = struct {
      pub fn dtor(env: beam.env, pid: *beam.pid) void {
          _ = beam.send(env, pid.*, .cleaned) catch unreachable;
      }
  };

  pub fn create_no_release(pid: beam.pid) PidResource {
      const resource = PidResource.create(pid, .{.released = false}) catch unreachable;
      return resource;
  }

  pub fn create_released(pid: beam.pid) PidResource {
      const resource = PidResource.create(pid, .{}) catch unreachable;
      return resource;
  }

  pub fn release(resource: PidResource) void {
    resource.release();
  }
  """

  test "you can create a released item" do
    this = self()

    spawn(fn ->
      create_released(this)
      :erlang.garbage_collect()
    end)

    assert_receive :cleaned, 100
  end

  test "you can create then release an item" do
    this = self()

    spawn(fn ->
      yo = create_no_release(this)
      release(yo)
      :erlang.garbage_collect()
    end)

    assert_receive :cleaned, 100
  end
end
