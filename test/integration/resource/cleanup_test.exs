defmodule ZiglerTest.Resource.CleanupTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    otp_app: :zigler,
    resources: [:PidResource],
    nifs: [
      :create_released,
      maybe_release: [args: [[cleanup: false], []]]
    ]

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

  pub fn maybe_release(resource: PidResource, release: bool) void {
    if (release) {
      resource.release();
    }
  }
  """

  test "a function call will keep resources, with no cleanup it doesn't release" do
    this = self()

    spawn(fn ->
      this
      |> create_released()
      |> maybe_release(false)
    end)

    refute_receive :cleaned, 500
  end

  test "a function call will keep resources, you can manually release" do
    this = self()

    spawn(fn ->
      this
      |> create_released()
      |> maybe_release(true)
    end)

    assert_receive :cleaned, 100
  end
end
