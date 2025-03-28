defmodule ZiglerTest.Callbacks.OnLoadMissingTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag ~w[on_load callbacks]a

  test "compiler error when named on_load function is missing" do
    assert_raise CompileError, "nofile: on_load callback foo not found", fn ->
      Code.compile_quoted(
        quote do
          defmodule ZiglerTest.OnLoadMissing do
            use Zig, otp_app: :zigler, callbacks: [on_load: :foo], dir: unquote(__DIR__)

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

  test "compiler error when default on_load function is missing" do
    assert_raise CompileError, "nofile: on_load callback not found", fn ->
      Code.compile_quoted(
        quote do
          defmodule ZiglerTest.OnLoadMissingDefault do
            use Zig, otp_app: :zigler, callbacks: [:on_load], dir: unquote(__DIR__)

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

  test "compiler error when on_load function is not pub" do
    assert_raise CompileError, "nofile:2: on_load callback foo must be declared `pub`", fn ->
      Code.compile_quoted(
        quote do
          defmodule ZiglerTest.OnLoadNotPub do
            use Zig, otp_app: :zigler, callbacks: [on_load: :foo], dir: unquote(__DIR__)

            ~Z"""
            const beam = @import("beam");
            fn foo(_: ?*?*u32, _: beam.term) c_int {
                return 0;
            }
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
