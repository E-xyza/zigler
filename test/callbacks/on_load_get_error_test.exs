defmodule ZiglerTest.Callbacks.OnLoadGetErrorTest do
  # this is a test of the "automatic" on_load function.

  use ZiglerTest.IntegrationCase, async: true

  @moduletag [on_load: true, callbacks: true]
  @moduletag :skip

  import ExUnit.CaptureLog

  test "error when on_load function is passed the wrong type" do
    log =
      capture_log(fn ->
        Code.compile_quoted(
          quote do
            defmodule ZiglerTest.OnLoadGetError do
              use Zig, otp_app: :zigler, callbacks: [on_load: :foo], dir: unquote(__DIR__)

              def __on_load__, do: "not_an_integer"

              ~Z"""
              const beam = @import("beam");
              pub fn foo(_: ?*?*u32, _: i32) void {}
              pub fn bar() void {}
              """
            end
          end
        )
      end)

    assert log =~ "[error] loading module Elixir.ZiglerTest.OnLoadGetError"
    assert log =~ "(-1)"
  end
end
