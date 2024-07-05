defmodule ZiglerTest.Callbacks.OnUpgradeAutomaticIntTest do
  # this is a test of the "automatic" on_upgrade function.  This means that the
  # beam.context.env variable is set, and the term value is set to beam.term.
  # the return value is also allowed to be an int value

  use ZiglerTest.IntegrationCase, async: true

  @moduletag :skip
  test "restore"

  import ExUnit.CaptureIO
  import ExUnit.CaptureLog

  def build_module(opts) do
    Code.compile_quoted(
      quote do
        defmodule ZiglerTest.OnUpgradeAutomaticInt do
          use Zig, otp_app: :zigler, callbacks: [on_upgrade: :on_upgrade], dir: unquote(__DIR__)

          defp __on_load__, do: unquote(opts)

          ~z"""
          const beam = @import("beam");
          const E = enum(c_int){ ok = 0, err = 42 };
          const S = struct{ pid: beam.pid, value: i32};

          pub fn on_upgrade(_: ?*?*u32, _: ?*?*u32, term: beam.term) i32 {
            const t = beam.get(S, term, .{}) catch unreachable;
            beam.send(t.pid, .{.result, t.value}, .{}) catch unreachable;
            return t.value;
          }
          pub fn bar() u8 { return #{unquote(opts[:value])}; }
          """
        end
      end
    )
  end

  alias ZiglerTest.OnUpgradeAutomaticInt

  test "on_upgrade generally works" do
    this = self()
    build_module(pid: this, value: 0)
    assert 0 = apply(OnUpgradeAutomaticInt, :bar, [])

    log_line =
      capture_log(fn ->
        redefine_warn =
          capture_io(:stderr, fn ->
            build_module(pid: self(), value: 42)
            assert_receive {:result, 42}
          end)

        assert redefine_warn =~ "redefining module ZiglerTest.OnUpgradeAutomaticInt"
      end)

    assert log_line =~ "Elixir.ZiglerTest.OnUpgradeAutomaticInt"
    assert log_line =~ "(42)"
  end
end
