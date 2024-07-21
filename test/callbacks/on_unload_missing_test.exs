defmodule ZiglerTest.Callbacks.OnUnloadMissingTest do
  use ZiglerTest.IntegrationCase, async: true

  test "compiler error when on_unload function is missing" do
    assert_raise CompileError, "nofile: on_unload callback foo not found", fn ->
      Code.compile_quoted(
        quote do
          defmodule ZiglerTest.OnunloadMissing do
            use Zig, otp_app: :zigler, callbacks: [on_unload: :foo], dir: unquote(__DIR__)

            ~Z"""
            pub fn bar() u8 {
                return 47;
            }
            """
          end
        end
      )
    end
  end

  test "compiler error when on_unload function is not pub" do
    assert_raise CompileError, "nofile:2: on_unload callback foo must be declared `pub`", fn ->
      Code.compile_quoted(
        quote do
          defmodule ZiglerTest.OnunloadNotPub do
            use Zig, otp_app: :zigler, callbacks: [on_unload: :foo], dir: unquote(__DIR__)

            ~Z"""
            const beam = @import("beam");
            fn foo(_: [*c]?*anyopaque) void {}
            pub fn bar() u8 {
                return 47;
            }
            """
          end
        end
      )
    end
  end
end
