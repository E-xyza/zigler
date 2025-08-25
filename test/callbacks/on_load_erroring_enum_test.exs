if System.get_env("DISABLE_TESTS", "false") == "true" do

defmodule ZiglerTest.Callbacks.OnLoadErroringEnumTest do
  # this is a test of the "automatic" on_load function.

  use ZiglerTest.IntegrationCase, async: true

  @moduletag [on_load: true, callbacks: true]
  @moduletag :skip

  import ExUnit.CaptureLog

  test "error when on_load function errors out" do
    log =
      capture_log(fn ->
        Code.compile_quoted(
          quote do
            defmodule ZiglerTest.OnLoadErroringEnum do
              use Zig, otp_app: :zigler, callbacks: [on_load: :foo], dir: unquote(__DIR__)

              ~Z"""
              const beam = @import("beam");
              const E = enum(c_int) { ok = 0, err = 42 };
              pub fn foo(_: ?*?*u32, _: beam.term) E {
                  return .err;
              }
              pub fn bar() u8 {
                  return 47;
              }
              """
            end
          end
        )
      end)

    assert log =~ "[error] loading module Elixir.ZiglerTest.OnLoadErroringEnum"
    assert log =~ "(42)"
  end
end

end
