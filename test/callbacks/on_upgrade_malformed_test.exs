defmodule ZiglerTest.Callbacks.OnUpgradeMalformedTest do
  # this is a test of the "automatic" on_upgrade function.

  use ZiglerTest.IntegrationCase, async: true

  @moduletag :on_upgrade

  test "compiler error when on_upgrade function has the wrong arity" do
    assert_raise CompileError,
                 "nofile:2: on_upgrade callback foo must have arity 3 or 4",
                 fn ->
                   Code.compile_quoted(
                     quote do
                       defmodule ZiglerTest.BadOnupgradeArity do
                         use Zig,
                           otp_app: :zigler,
                           callbacks: [on_upgrade: :foo],
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

  describe "compiler error when on_upgrade arity 3" do
    test "has an unusable first parameter" do
      assert_raise CompileError,
                   "nofile:2: on_upgrade (automatic-style) callback foo must have a first parameter of type `?*?*`.\n\n    got: `beam.env`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgradeAutomaticFirstParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_upgrade: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: beam.env, _: f32, _: f32) void {}
                           pub fn bar() u8 {
                               return 47;
                           }
                           """
                         end
                       end
                     )
                   end
    end

    test "has an unusable second parameter" do
      assert_raise CompileError,
                   "nofile:2: on_upgrade (automatic-style) callback foo must have a second parameter of type `?*?*`.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgradeAutomaticSecondParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_upgrade: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: ?*?*f32, _: f32, _: f32) void {}
                           pub fn bar() u8 {
                               return 47;
                           }
                           """
                         end
                       end
                     )
                   end
    end

    test "has an unusable third parameter" do
      assert_raise CompileError,
                   "nofile:2: on_upgrade (automatic-style) callback foo must have a third parameter of a type compatible with `beam.get`.\n\n    got: `?*?*f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgradeAutomaticThirdParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_upgrade: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: ?*?*anyopaque, _: ?*?*anyopaque, _: ?*?*f32) void {}
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
                   "nofile:2: on_upgrade (automatic-style) callback must have an integer, enum, `void`, or `!void` as a return.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgradeAutomaticReturn do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [:on_upgrade],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn on_upgrade(_: ?*?*u32, _: ?*?*u32, _: beam.term) f32 {
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

  describe "compiler error when on_upgrade arity 4" do
    test "has the wrong first parameter" do
      assert_raise CompileError,
                   "nofile:3: on_upgrade (raw-style) callback foo must have a first parameter of type `beam.env`.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgradeRawFirstParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_upgrade: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           const e = @import("erl_nif");
                           pub fn foo(_: f32, _: ?*?*u32, _: ?*?*u32, _: e.ErlNifTerm) c_int {
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
                   "nofile:3: on_upgrade (raw-style) callback foo must have a second parameter of type `?*?*`.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgradeRawSecondParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_upgrade: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           const e = @import("erl_nif");
                           pub fn foo(_: beam.env, _: f32, _: ?*?*u32, _: e.ErlNifTerm) c_int {
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
                   "nofile:3: on_upgrade (raw-style) callback foo must have a third parameter of type `?*?*`.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgradeRawThirdParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_upgrade: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           const e = @import("erl_nif");
                           pub fn foo(_: beam.env, _: ?*?*f32, _: f32, _: e.ErlNifTerm) c_int {
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

    test "has the wrong fourth parameter" do
      assert_raise CompileError,
                   "nofile:3: on_upgrade (raw-style) callback foo must have a fourth parameter of type `e.ErlNifTerm`.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgradeRawFourthParameter do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_upgrade: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           const e = @import("erl_nif");
                           pub fn foo(_: beam.env, _: ?*?*f32, _: ?*?*u32, _: f32) c_int {
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
                   "nofile:3: on_upgrade (raw-style) callback must have an `c_int` as a return.\n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgradeRawReturn do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [:on_upgrade],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           const e = @import("erl_nif");
                           pub fn on_upgrade(_: beam.env, _: ?*?*u32, _: ?*?*u32, _: e.ErlNifTerm) f32 {
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
