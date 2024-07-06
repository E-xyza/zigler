defmodule ZiglerTest.Callbacks.OnUpgradeAutomaticGetTest do
  # this is a test of the "automatic" on_upgrade function.  This means that the
  # beam.context.env variable is set, and the term value is set to beam.term.
  # the return value is also allowed to be a void value

  use ZiglerTest.IntegrationCase, async: true

  import ExUnit.CaptureIO

  def build_module(opts) do
    Code.compile_quoted(
      quote do
        defmodule ZiglerTest.OnUpgradeAutomaticGet do
          use Zig, otp_app: :zigler, callbacks: [on_upgrade: :on_upgrade], dir: unquote(__DIR__)

          defp __on_load__, do: unquote(opts)

          ~z"""
          const beam = @import("beam");
          pub const S = struct{ pid: beam.pid, value: i32};

          pub fn on_upgrade(_: ?*?*u32, _: ?*?*u32, config: S) void {
            beam.send(config.pid, .{.result, config.value}, .{}) catch unreachable;
          }
          pub fn bar() u8 { return #{unquote(opts[:value])}; }
          """
        end
      end
    )
  end

  alias ZiglerTest.OnUpgradeAutomaticGet

  test "on_upgrade generally works" do
    this = self()
    build_module(pid: this, value: 0)
    assert 0 = apply(OnUpgradeAutomaticGet, :bar, [])

    redefine_warn =
      capture_io(:stderr, fn ->
        build_module(pid: self(), value: 42)
        assert_receive {:result, 42}
      end)

    assert redefine_warn =~ "redefining module ZiglerTest.OnUpgradeAutomaticGet"
  end
end
