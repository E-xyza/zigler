defmodule ZiglerTest.Callbacks.OnUnloadMalformedTest do
  # this is a test of the "automatic" on_unload function.

  use ZiglerTest.IntegrationCase, async: true

  @moduletag [on_unload: true, callbacks: true]
  @moduletag :skip

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
                         pub fn bar() u8 {
                             return 47;
                         }
                         """
                       end
                     end
                   )
                 end
  end

  describe "compiler error when on_unload arity 1" do
    test "has the wrong parameter" do
      assert_raise CompileError,
                   "nofile:2: on_unload (automatic-style) callback foo must have a parameter of type `?*`.\n\n    got: `beam.env`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnunloadAutoParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_unload: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: beam.env) void {}
                           pub fn bar() u8 {
                               return 47;
                           }
                           """
                         end
                       end
                     )
                   end
    end

    test "has the wrong return" do
      assert_raise CompileError,
                   "nofile:2: on_unload (automatic-style) callback must have `void` as a return.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnunloadAutoReturn do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [:on_unload],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn on_unload(_: ?*anyopaque) f32 {
                               return 0.0;
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

  describe "compiler error when on_unload arity 2" do
    test "has the wrong first parameter" do
      assert_raise CompileError,
                   "nofile:2: on_unload (raw-style) callback foo must have a first parameter of type `beam.env`.\n\n    got: `?*anyopaque`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnunloadRawFirstParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_unload: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: ?*anyopaque, _: ?*anyopaque) void {
                               return 0.0;
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

    test "has the wrong second parameter" do
      assert_raise CompileError,
                   "nofile:2: on_unload (raw-style) callback must have a second parameter of type `?*`.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnunloadRawSecondParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [:on_unload],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn on_unload(_: beam.env, _: f32) void {
                               return 0.0;
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

    test "has the wrong return" do
      assert_raise CompileError,
                   "nofile:3: on_unload (raw-style) callback foo must have `void` as a return.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnunloadRawReturn do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_unload: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           const e = @import("erl_nif");
                           pub fn foo(_: beam.env, _: ?*anyopaque) f32 {
                               return 0.0;
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
end
