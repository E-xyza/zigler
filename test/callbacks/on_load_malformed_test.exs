defmodule ZiglerTest.Callbacks.OnLoadMalformedTest do
  # this is a test of the "automatic" on_load function.

  use ZiglerTest.IntegrationCase, async: true

  @moduletag [on_load: true, callbacks: true]
  @moduletag :skip

  test "compiler error when on_load function has the wrong arity" do
    assert_raise CompileError,
                 "nofile:2: on_load callback foo must have arity 2 or 3",
                 fn ->
                   Code.compile_quoted(
                     quote do
                       defmodule ZiglerTest.BadOnloadArity do
                         use Zig,
                           otp_app: :zigler,
                           callbacks: [on_load: :foo],
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

  describe "compiler error when on_load arity 2" do
    test "has the wrong first parameter" do
      assert_raise CompileError,
                   "nofile:2: on_load (automatic-style) callback foo must have a first paramater of a `?*?*` type.\n\n    got: `i32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnloadAutoFirstParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_load: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: i32, _: i32) void {}
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
                   "nofile:2: on_load (automatic-style) callback foo must have a second parameter of a type compatible with `beam.get`.\n\n    got: `beam.env`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnloadAutoSecondParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_load: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: ?*?*u32, _: beam.env) void {}
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
                   "nofile:2: on_load (automatic-style) callback must have a return of type integer, enum, `void`, or `!void`.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnloadAutoReturn do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [:on_load],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn on_load(_: ?*?*anyopaque, _: beam.term) f32 {
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

  describe "compiler error when on_load arity 3" do
    test "has the wrong first parameter" do
      assert_raise CompileError,
                   "nofile:2: on_load (raw-style) callback foo must have a first parameter of type `beam.env`.\n\n    got: `u32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnloadRawFirstParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_load: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: u32, _: ?*?*u32, _: f32) c_int {
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
                   "nofile:3: on_load (raw-style) callback foo must have a second parameter of type `?*?*`.\n\n    got: `u32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnloadRawSecondParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_load: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           const e = @import("erl_nif");
                           pub fn foo(_: beam.env, _: u32, _: e.ErlNifTerm) c_int {
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

    test "has the wrong third parameter" do
      assert_raise CompileError,
                   "nofile:3: on_load (raw-style) callback foo must have a third parameter of type `e.ErlNifTerm`.\n\n    got: `u32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnloadRawThirdParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_load: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           const e = @import("erl_nif");
                           pub fn foo(_: beam.env, _: ?*?*u32, _: u32) c_int {
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
                   "nofile:3: on_load (raw-style) callback foo must have return type `c_int`.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnloadRawReturn do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_load: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           const e = @import("erl_nif");
                           pub fn foo(_: beam.env, _: ?*?*u32, _: e.ErlNifTerm) f32 {
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
