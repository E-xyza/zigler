defmodule ZiglerTest.Resource.BasicTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, resources: ["beam.term": [dtor: :destroy]]

  ~Z"""
  const beam = @import("beam");
  const Resource = beam.Resource;
  pub const resource = beam.resources(@import("root"));

  pub fn destroy(env: beam.env, pid: Resource(beam.pid)) void {
      beam.send(env, pid, .ok);
  }

  pub fn create_then_release(pid: beam.pid) Resource(beam.pid) {
      const result = resource.create(pid, .{}) catch unreachable;
      resource.release(result);
      return result;
  }

  pub fn create_released(pid: beam.pid) Resource(beam.pid) {
      const result = resource.create_released(pid, .{}) catch unreachable;
      return result;
  }

  pub fn keep(res: Resource(beam.pid)) void {
      resource.keep(res);
  }
  """

  test "you can create then release an item" do
    this = self()

    spawn(fn ->
      create_then_release(this)
      :erlang.garbage_collect()
    end)

    assert_receive 1000, :ok
  end

  test "you can create a released item" do
    this = self()

    spawn(fn ->
      create_released(this)
      :erlang.garbage_collect()
    end)

    assert_receive 1000, :ok
  end

  test "if the item has been kept, it won't release" do
    this = self()

    spawn(fn ->
      this
      |> create_then_release
      |> keep

      :erlang.garbage_collect()
    end)

    refute_receive 1000, :ok
  end
end
