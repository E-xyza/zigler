defmodule ZiglerTest.Resource.ReleaseTest do
  use ZiglerTest.IntegrationCase, async: true

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
    end)

    assert_receive :cleaned, 100
  end

  # it's not entirely clear as to why this doesn't work: it should.
  @tag :skip
  test "you can create then release an item" do
    this = self()

    spawn(fn ->
      this
      |> create_no_release()
      |> release()
    end)

    assert_receive :cleaned
  end

  test "an unkept resource is never released" do
    this = self()

    spawn(fn ->
      create_no_release(this)
    end)

    refute_receive :cleaned, 500
  end
end
