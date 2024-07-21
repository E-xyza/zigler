defmodule ZiglerTest.Types.OtherTest do
  use ZiglerTest.IntegrationCase, async: true

  describe "beam.env" do
    test "cannot generally be used as a parameter" do
      assert_raise CompileError,
                   "nofile:2: nif function `forbidden` cannot have a value of type beam.env as a parameter",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.Types.BeamEnvFails do
                           use Zig, otp_app: :zigler, dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn forbidden(env: beam.env) void {
                               _ = env;
                           }
                           """
                         end
                       end
                     )
                   end
    end
  end
end
