defmodule ZiglerTest.Callbacks.OnUnloadAutomaticTest do
  # this is a test of the "automatic" on_unload function.  This means that the
  # beam.context.env variable is set, and the return value must be void.

  use ZiglerTest.IntegrationCase, async: true

  @moduletag [on_unload: true, callbacks: true]

  def build_module do
    Code.compile_quoted(
      quote do
        defmodule ZiglerTest.OnUnloadAutomatic do
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

          pub fn on_unload(_: ?*u32) void {
              beam.send(pid, .unloaded, .{}) catch unreachable;
          }

          pub fn bar() void {}
          """
        end
      end
    )
  end

  alias ZiglerTest.OnUnloadAutomatic

  test "on_unload generally works" do
    this = self()
    Application.put_env(:zigler, OnUnloadAutomatic, this)

    build_module()
    assert :ok = apply(OnUnloadAutomatic, :bar, [])

    :code.delete(OnUnloadAutomatic)
    :code.purge(OnUnloadAutomatic)

    assert_receive :unloaded, 1000
  end
end
