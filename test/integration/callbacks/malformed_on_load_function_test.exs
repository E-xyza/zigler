defmodule ZiglerTest.Callbacks.MalformedOnLoadFunction do
  # this is a test of the "automatic" on_load function.

  use ZiglerTest.IntegrationCase, async: true

  test "compiler error when on_load function has the wrong arity" do
    assert_raise CompileError,
                 "nofile:2: on_load callback foo must have arity 2 or 3",
                 fn ->
                   Code.compile_quoted(
                     quote do
                       defmodule ZiglerTest.BadOnloadArity do
                         use Zig, otp_app: :zigler, callbacks: [on_load: :foo]

                         ~Z"""
                         const beam = @import("beam");
                         pub fn foo(_: beam.env) void {}
                         pub fn bar() u8 { return 47; }
                         """
                       end
                     end
                   )
                 end
  end

  describe "compiler error when on_load arity 2" do
    test "has the wrong parameters" do
      assert_raise CompileError,
                   "nofile:2: on_load callback foo with arity 2 must have `[*c]?*anyopaque` and `beam.term` as parameters. \n\n    got: `beam.env`\n\n    and: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnload2Parameters do
                           use Zig, otp_app: :zigler, callbacks: [on_load: :foo]

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: beam.env, _: f32) void {}
                           pub fn bar() u8 { return 47; }
                           """
                         end
                       end
                     )
                   end
    end

    test "has the wrong return" do
      assert_raise CompileError,
                   "nofile:2: on_load callback foo with arity 2 must have an integer, enum, or `void` as a return. \n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnload2Return do
                           use Zig, otp_app: :zigler, callbacks: [on_load: :foo]

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: [*c]?*anyopaque, _: beam.term) f32 { return 0.0; }
                           pub fn bar() u8 { return 47; }
                           """
                         end
                       end
                     )
                   end
    end
  end
end
