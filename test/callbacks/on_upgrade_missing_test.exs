defmodule ZiglerTest.Callbacks.OnUpgradeMissingTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag [on_upgrade: true, callbacks: true]
  @moduletag :skip

  test "compiler error when on_upgrade function is missing" do
    assert_raise CompileError, "nofile: on_upgrade callback foo not found", fn ->
      Code.compile_quoted(
        quote do
          defmodule ZiglerTest.OnUpgradeMissing do
            use Zig, otp_app: :zigler, callbacks: [on_upgrade: :foo], dir: unquote(__DIR__)

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

  test "compiler error when on_upgrade function is not pub" do
    assert_raise CompileError, "nofile:2: on_upgrade callback foo must be declared `pub`", fn ->
      Code.compile_quoted(
        quote do
          defmodule ZiglerTest.OnUpgradeNotPub do
            use Zig, otp_app: :zigler, callbacks: [on_upgrade: :foo], dir: unquote(__DIR__)

            ~Z"""
            const beam = @import("beam");
            fn foo(_: [*c]?*anyopaque, _: ?*?*u32, _: beam.term) c_int {
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
