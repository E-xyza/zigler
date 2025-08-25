defmodule ZiglerTest.Callbacks.OnUpgradeRawTest do
  # this is a test of the "raw" on_upgrade function.  This means that the
  # beam.context.env variable is set, and the term value is set to beam.term.
  # the return value is also allowed to be an int value

  use ZiglerTest.IntegrationCase, async: true

  @moduletag [on_upgrade: true, callbacks: true]
  @moduletag :skip

  import ExUnit.CaptureIO
  import ExUnit.CaptureLog

  def build_module(opts) do
    Code.compile_quoted(
      quote do
        defmodule ZiglerTest.OnUpgradeRaw do
          use Zig, otp_app: :zigler, callbacks: [on_upgrade: :on_upgrade], dir: unquote(__DIR__)

          defp __on_load__, do: unquote(opts)

          ~z"""
          const beam = @import("beam");
          const e = @import("erl_nif");

          const S = struct{ pid: beam.pid, value: i32};
          pub fn on_upgrade(env: beam.env, _: ?*?*u32, _: ?*?*u32, term: e.ErlNifTerm) i32 {
            const t = beam.get(S, .{.v = term}, .{.env = env}) catch unreachable;
            beam.send(t.pid, .{.result, t.value}, .{.env = env}) catch unreachable;
            return t.value;
          }
          pub fn bar() u8 { return #{unquote(opts[:value])}; }
          """
        end
      end
    )
  end

  alias ZiglerTest.OnUpgradeRaw

  test "on_upgrade generally works" do
    this = self()
    build_module(pid: this, value: 0)
    assert 0 = apply(OnUpgradeRaw, :bar, [])

    log_line =
      capture_log(fn ->
        redefine_warn =
          capture_io(:stderr, fn ->
            build_module(pid: self(), value: 42)
            assert_receive {:result, 42}
          end)

        assert redefine_warn =~ "redefining module ZiglerTest.OnUpgradeRaw"
      end)

    assert log_line =~ "Elixir.ZiglerTest.OnUpgradeRaw"
    assert log_line =~ "(42)"
  end
end
