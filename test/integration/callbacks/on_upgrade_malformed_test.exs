defmodule ZiglerTest.Callbacks.OnUpgradeMalformedTest do
  # this is a test of the "automatic" on_upgrade function.

  use ZiglerTest.IntegrationCase, async: true

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
                         pub fn bar() u8 { return 47; }
                         """
                       end
                     end
                   )
                 end
  end

  describe "compiler error when on_upgrade arity 3" do
    test "has the wrong parameters" do
      assert_raise CompileError,
                   "nofile:2: on_upgrade callback foo with arity 3 must have `?*?*u32`, `?*?*u32` and `beam.term` as parameters. \n\n    got: `beam.env`\n\n    and: `f32`\n\n    and: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgrade3Parameters do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_upgrade: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: beam.env, _: f32, _: f32) void {}
                           pub fn bar() u8 { return 47; }
                           """
                         end
                       end
                     )
                   end
    end

    test "has the wrong return" do
      assert_raise CompileError,
                   "nofile:2: on_upgrade callback foo with arity 3 must have an integer, enum, `void`, or `!void` as a return. \n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgrade3Return do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_upgrade: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: ?*?*u32, _: ?*?*u32, _: beam.term) f32 { return 0.0; }
                           pub fn bar() u8 { return 47; }
                           """
                         end
                       end
                     )
                   end
    end

    test "uses a an non-pub struct"

    test "uses an unapproved type"
  end

  describe "compiler error when on_upgrade arity 4" do
    test "has the wrong parameters" do
      assert_raise CompileError,
                   "nofile:2: on_upgrade callback foo with arity 4 must have `beam.env`, `?*?*u32`, `?*?*u32` and `e.ErlNifTerm` as parameters. \n\n    got: `beam.env`\n\n    and: `[*c]?*anyopaque`\n\n    and: `[*c]?*anyopaque`\n\n    and: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgrade4Parameters do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_upgrade: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           pub fn foo(_: beam.env, _: ?*?*u32, _: ?*?*u32, _: f32) c_int { return 0.0; }
                           pub fn bar() u8 { return 47; }
                           """
                         end
                       end
                     )
                   end
    end

    test "has the wrong return" do
      assert_raise CompileError,
                   "nofile:3: on_upgrade callback foo with arity 4 must have an `c_int` as a return. \n\n    got: `f32`",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.BadOnupgrade4Return do
                           use Zig,
                             otp_app: :zigler,
                             callbacks: [on_upgrade: :foo],
                             dir: unquote(__DIR__)

                           ~Z"""
                           const beam = @import("beam");
                           const e = @import("erl_nif");
                           pub fn foo(_: beam.env, _: ?*?*u32, _: ?*?*u32, _: e.ErlNifTerm) f32 { return 0.0; }
                           pub fn bar() u8 { return 47; }
                           """
                         end
                       end
                     )
                   end
    end
  end
end
