defmodule ZiglerTest.Callbacks.OnUpgradeAutomaticVoidTest do
  # this is a test of the "automatic" on_upgrade function.  This means that the
  # beam.context.env variable is set, and the term value is set to beam.term.
  # the return value is also allowed to be a void value

  use ZiglerTest.IntegrationCase, async: true

  import ExUnit.CaptureIO

  def build_module(value) do
    Application.put_env(:zigler, ZiglerTest.OnUpgradeAutomaticVoid, pid: self(), value: value)

    Code.compile_quoted(
      quote do
        defmodule ZiglerTest.OnUpgradeAutomaticVoid do
          use Zig, otp_app: :zigler, callbacks: [:on_upgrade], dir: unquote(__DIR__)

          defp __on_load__ do
            Application.get_env(:zigler, __MODULE__)
          end

          ~z"""
          const beam = @import("beam");
          const S = struct{ pid: beam.pid, value: i32};
          const std = @import("std");

          pub fn on_upgrade(_: ?*?*u32, _: ?*?*u32, term: beam.term) void {
            const t = beam.get(S, term, .{}) catch unreachable;
            beam.send(t.pid, .{.result, t.value}, .{}) catch unreachable;
          }
          pub fn bar() u8 { return #{unquote(value)}; }
          """
        end
      end
    )
  end

  alias ZiglerTest.OnUpgradeAutomaticVoid

  test "on_upgrade generally works" do
    build_module(0)
    assert 0 = apply(OnUpgradeAutomaticVoid, :bar, [])

    redefine_warn =
      capture_io(:stderr, fn ->
        build_module(42)
        assert_receive {:result, 42}, 10000
      end)

    assert redefine_warn =~ "redefining module ZiglerTest.OnUpgradeAutomaticVoid"
  end
end
