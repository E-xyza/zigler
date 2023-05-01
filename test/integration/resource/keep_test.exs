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

  pub fn create_released(pid: beam.pid) PidResource {
      const resource = PidResource.create(pid, .{}) catch unreachable;
      return resource;
  }

  pub fn keep(resource: PidResource) void {
    resource.keep();
  }
  """

  test "you can create then release an item" do
    this = self()

    spawn(fn -> create_released(this) end)

    assert_receive :cleaned, 100
  end

  test "an unkept resource is never released" do
    this = self()

    spawn(fn ->
      this
      |> create_released()
      |> keep()
    end)

    refute_receive :cleaned, 500
  end
end
