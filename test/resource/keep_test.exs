defmodule ZiglerTest.Resource.KeepTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, resources: [:PidResource]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  const Resource = beam.Resource;

  pub const PidResource = Resource(beam.pid, @import("root"), .{ .Callbacks = PidResourceCallbacks });

  pub const PidResourceCallbacks = struct {
      pub fn dtor(pid: *beam.pid) void {
          beam.send(pid.*, .cleaned, .{ .clear = false }) catch unreachable;
      }
  };

  pub fn create_released(pid: beam.pid) PidResource {
      const resource = PidResource.create(pid, .{}) catch unreachable;
      return resource;
  }

  pub fn keep(resource: PidResource) void {
      resource.keep();
  }

  pub fn manual_keep(term: beam.term, should_keep: bool) void {
      const resource = beam.get(PidResource, term, .{ .keep = false }) catch unreachable;
      if (should_keep) {
          resource.keep();
      }
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

  describe "you can manually use beam.get to" do
    test "decide to not keep" do
      this = self()

      spawn(fn ->
        this
        |> create_released()
        |> manual_keep(false)
      end)

      assert_receive :cleaned, 100
    end

    test "decide to keep" do
      this = self()

      spawn(fn ->
        this
        |> create_released()
        |> manual_keep(true)
      end)

      refute_receive :cleaned, 500
    end
  end
end
