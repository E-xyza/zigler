defmodule ZiglerTest.Callbacks.OnUpgradeAutomaticEnumTest do
  # this is a test of the "automatic" on_upgrade function.  This means that the
  # beam.context.env variable is set, and the term value is set to beam.term.
  # the return value is also allowed to be an enum value

  use ZiglerTest.IntegrationCase, async: true

  @moduletag [on_upgrade: true, callbacks: true]
  @moduletag :skip

  import ExUnit.CaptureIO
  import ExUnit.CaptureLog

  def build_module(opts) do
    Code.compile_quoted(
      quote do
        defmodule ZiglerTest.OnUpgradeAutomaticEnum do
          use Zig, otp_app: :zigler, callbacks: [on_upgrade: :on_upgrade], dir: unquote(__DIR__)

          defp __on_load__, do: unquote(opts)

          ~z"""
          const beam = @import("beam");
          const E = enum(c_int){ ok = 0, err = 42 };
          const S = struct{ pid: beam.pid, value: u32};

          pub fn on_upgrade(_: ?*?*u32, _: ?*?*u32, term: beam.term) E {
            const t = beam.get(S, term, .{}) catch unreachable;
            beam.send(t.pid, .{.result, t.value}, .{}) catch unreachable;
            return if (t.value == 42) .err else .ok;
          }
          pub fn bar() u8 { return #{unquote(opts[:value])}; }
          """
        end
      end
    )
  end

  alias ZiglerTest.OnUpgradeAutomaticEnum

  test "on_upgrade generally works" do
    this = self()
    build_module(pid: this, value: 0)
    assert 0 = apply(OnUpgradeAutomaticEnum, :bar, [])

    redefine_warn =
      capture_io(:stderr, fn ->
        build_module(pid: this, value: 47)
        assert_receive {:result, 47}, 1000
      end)

    assert redefine_warn =~ "redefining module ZiglerTest.OnUpgradeAutomaticEnum"

    log_line =
      capture_log(fn ->
        redefine_warn =
          capture_io(:stderr, fn ->
            build_module(pid: self(), value: 42)
            assert_receive {:result, 42}
          end)

        assert redefine_warn =~ "redefining module ZiglerTest.OnUpgradeAutomaticEnum"
      end)

    assert log_line =~ "Elixir.ZiglerTest.OnUpgradeAutomaticEnum"
    assert log_line =~ "(42)"
  end
end
