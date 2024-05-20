defmodule ZiglerTest.Callbacks.OnUnloadMalformedTest do
  # this is a test of the "automatic" on_unload function.

  use ZiglerTest.IntegrationCase, async: true

  test "compiler error when on_unload function has the wrong arity" do
    assert_raise CompileError,
                 "nofile:2: on_unload callback foo must have arity 1 or 2",
                 fn ->
                   Code.compile_quoted(
                     quote do
                       defmodule ZiglerTest.BadUnloadArity do
                         use Zig,
                           otp_app: :zigler,
                           callbacks: [on_unload: :foo],
                           dir: unquote(__DIR__)

                         ~Z"""
                         const beam = @import("beam");
                         pub fn foo() void {}
                         pub fn bar() u8 { return 47; }
                         """
                       end
                     end
                   )
                 end
  end

  describe "compiler error when on_unload arity 1" do
    test "has the wrong parameters" do
      assert_raise CompileError,
                   "nofile:2: on_unload callback foo with arity 1 must have `?*anyopaque` as a parameter. \n\n    got: `beam.env`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnunload1Parameters do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_unload: :foo],
                             dir: unquote(__DIR__)

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

    test "has the wrong return" do
      assert_raise CompileError,
                   "nofile:2: on_unload callback foo with arity 1 must have `void` as a return. \n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnunload1Return do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_unload: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: ?*anyopaque) f32 { return 0.0; }
                           pub fn bar() u8 { return 47; }
                           """
                         end
                       end
                     )
                   end
    end
  end

  describe "compiler error when on_unload arity 2" do
    test "has the wrong parameters" do
      assert_raise CompileError,
                   "nofile:2: on_unload callback foo with arity 2 must have `beam.env` and `?*anyopaque` as parameters. \n\n    got: `?*anyopaque`\n\n    and: `?*anyopaque`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnload2Parameters do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_unload: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: ?*anyopaque, _: ?*anyopaque) void { return 0.0; }
                           pub fn bar() u8 { return 47; }
                           """
                         end
                       end
                     )
                   end
    end

    test "has the wrong return" do
      assert_raise CompileError,
                   "nofile:3: on_unload callback foo with arity 2 must have `void` as a return. \n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnload2Return do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_unload: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           const e = @import("erl_nif");
                           pub fn foo(_: beam.env, _: ?*anyopaque) f32 { return 0.0; }
                           pub fn bar() u8 { return 47; }
                           """
                         end
                       end
                     )
                   end
    end
  end
end
