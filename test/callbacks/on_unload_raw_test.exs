defmodule ZiglerTest.Callbacks.OnUnloadRawTest do
  use ZiglerTest.IntegrationCase, async: true

  def build_module do
    Code.compile_quoted(
      quote do
        defmodule ZiglerTest.OnUnloadRaw do
          use Zig,
            otp_app: :zigler,
            callbacks: [on_load: :on_load, on_unload: :on_unload],
            dir: unquote(__DIR__)

          def __on_load__, do: Application.fetch_env!(:zigler, __MODULE__)

          ~Z"""
          const beam = @import("beam");

          var pid: beam.pid = undefined;

          pub fn on_load(_: ?*?*u32, term: beam.term) void {
              pid = beam.get(beam.pid, term, .{}) catch unreachable;
          }

          pub fn on_unload(env: beam.env, _: ?*anyopaque) void {
              beam.send(pid, .unloaded, .{ .env = env }) catch unreachable;
          }

          pub fn bar() void {}
          """
        end
      end
    )
  end

  alias ZiglerTest.OnUnloadRaw

  test "on_unload generally works" do
    this = self()
    Application.put_env(:zigler, OnUnloadRaw, this)

    build_module()
    assert :ok = apply(OnUnloadRaw, :bar, [])

    :code.delete(OnUnloadRaw)
    :code.purge(OnUnloadRaw)

    assert_receive :unloaded, 1000
  end
end
