defmodule ZiglerTest.Snapshot.FooterTest do
  use ExUnit.Case, async: true

  alias Zigler.{Code, Module, Parser.Nif, Parser.Resource}

  @moduletag :snapshot

  describe "the zigler compiler footer generates" do
    test "works for a single function" do

      [major, minor] = Code.nif_major_minor()

      assert """
      // footer for Elixir.Foo in foo.exs:

      export var __exported_nifs__ = [_]e.ErlNifFunc{
        e.ErlNifFunc{
          .name = "foo",
          .arity = 0,
          .fptr = __foo_shim__,
          .flags = 0,
        },
      };

      const entry = e.ErlNifEntry{
        .major = #{major},
        .minor = #{minor},
        .name = "Elixir.Foo",
        .num_of_funcs = 1,
        .funcs = &(__exported_nifs__[0]),
        .load = beam.blank_load,
        .reload = beam.blank_load,     // currently unsupported
        .upgrade = beam.blank_upgrade, // currently unsupported
        .unload = beam.blank_unload,   // currently unsupported
        .vm_variant = "beam.vanilla",
        .options = 1,
        .sizeof_ErlNifResourceTypeInit = @sizeOf(e.ErlNifResourceTypeInit),
        .min_erts = "erts-#{:erlang.system_info(:version)}"
      };

      export fn nif_init() *const e.ErlNifEntry{
        return &entry;
      }
      """ == %Module{nifs: [%Nif{name: :foo, arity: 0}], file: "foo.exs", module: Foo, otp_app: :zigler}
      |> Code.footer
      |> IO.iodata_to_binary
    end

    test "works for a function + a resource" do

      [major, minor] = Code.nif_major_minor()

      assert """
      // footer for Elixir.Foo in foo.exs:

      export var __exported_nifs__ = [_]e.ErlNifFunc{
        e.ErlNifFunc{
          .name = "foo",
          .arity = 0,
          .fptr = __foo_shim__,
          .flags = 0,
        },
      };

      var __bar_resource__: beam.resource_type = undefined;

      fn __init_bar_resource__(env: beam.env) beam.resource_type {
        return e.enif_open_resource_type(
          env,
          null,
          "bar",
          __destroy_bar__,
          @intToEnum(e.ErlNifResourceFlags, 3),
          null);
      }

      export fn __destroy_bar__(env: beam.env, res: ?*c_void) void {}

      fn __resource_type__(comptime T: type) beam.resource_type {
        switch (T) {
          bar => return __bar_resource__,
          else => unreachable
        }
      }

      const __resource__ = struct {
        fn create(comptime T: type, env: beam.env, value: T) !beam.term {
          return beam.resource.create(T, env, __resource_type__(T), value);
        }

        fn update(comptime T: type, env: beam.env, res: beam.term, value: T) !void {
          return beam.resource.update(T, env, __resource_type__(T), res, value);
        }

        fn fetch(comptime T: type, env: beam.env, res: beam.term) !T {
          return beam.resource.fetch(T, env, __resource_type__(T), res);
        }

        fn keep(comptime T: type, env: beam.env, res: beam.term) !void {
          return beam.resource.keep(T, env, __resource_type__(T), res);
        }

        fn release(comptime T: type, env: beam.env, res: beam.term) void {
          return beam.resource.release(env, __resource_type__(T), res);
        }
      };

      export fn nif_load(env: beam.env, priv: [*c]?*c_void, load_info: beam.term) c_int {
        __bar_resource__ = __init_bar_resource__(env);
        return 0;
      }

      const entry = e.ErlNifEntry{
        .major = #{major},
        .minor = #{minor},
        .name = "Elixir.Foo",
        .num_of_funcs = 1,
        .funcs = &(__exported_nifs__[0]),
        .load = nif_load,
        .reload = beam.blank_load,     // currently unsupported
        .upgrade = beam.blank_upgrade, // currently unsupported
        .unload = beam.blank_unload,   // currently unsupported
        .vm_variant = "beam.vanilla",
        .options = 1,
        .sizeof_ErlNifResourceTypeInit = @sizeOf(e.ErlNifResourceTypeInit),
        .min_erts = "erts-#{:erlang.system_info(:version)}"
      };

      export fn nif_init() *const e.ErlNifEntry{
        return &entry;
      }
      """ == %Module{nifs: [%Nif{name: :foo, arity: 0}],
                     resources: [%Resource{name: :bar}],
                     file: "foo.exs",
                     module: Foo,
                     otp_app: :zigler}
      |> Code.footer
      |> IO.iodata_to_binary
    end

    test "works for a function + a resource + cleanup" do

      [major, minor] = Code.nif_major_minor()

      assert """
      // footer for Elixir.Foo in foo.exs:

      export var __exported_nifs__ = [_]e.ErlNifFunc{
        e.ErlNifFunc{
          .name = "foo",
          .arity = 0,
          .fptr = __foo_shim__,
          .flags = 0,
        },
      };

      var __bar_resource__: beam.resource_type = undefined;

      fn __init_bar_resource__(env: beam.env) beam.resource_type {
        return e.enif_open_resource_type(
          env,
          null,
          "bar",
          __destroy_bar__,
          @intToEnum(e.ErlNifResourceFlags, 3),
          null);
      }

      export fn __destroy_bar__(env: beam.env, res: ?*c_void) void {
        if (res) |__res__| {
          baz(env, @ptrCast(*bar, @alignCast(@alignOf(*bar), __res__)));
        } else unreachable;
      }

      fn __resource_type__(comptime T: type) beam.resource_type {
        switch (T) {
          bar => return __bar_resource__,
          else => unreachable
        }
      }

      const __resource__ = struct {
        fn create(comptime T: type, env: beam.env, value: T) !beam.term {
          return beam.resource.create(T, env, __resource_type__(T), value);
        }

        fn update(comptime T: type, env: beam.env, res: beam.term, value: T) !void {
          return beam.resource.update(T, env, __resource_type__(T), res, value);
        }

        fn fetch(comptime T: type, env: beam.env, res: beam.term) !T {
          return beam.resource.fetch(T, env, __resource_type__(T), res);
        }

        fn keep(comptime T: type, env: beam.env, res: beam.term) !void {
          return beam.resource.keep(T, env, __resource_type__(T), res);
        }

        fn release(comptime T: type, env: beam.env, res: beam.term) void {
          return beam.resource.release(env, __resource_type__(T), res);
        }
      };

      export fn nif_load(env: beam.env, priv: [*c]?*c_void, load_info: beam.term) c_int {
        __bar_resource__ = __init_bar_resource__(env);
        return 0;
      }

      const entry = e.ErlNifEntry{
        .major = #{major},
        .minor = #{minor},
        .name = "Elixir.Foo",
        .num_of_funcs = 1,
        .funcs = &(__exported_nifs__[0]),
        .load = nif_load,
        .reload = beam.blank_load,     // currently unsupported
        .upgrade = beam.blank_upgrade, // currently unsupported
        .unload = beam.blank_unload,   // currently unsupported
        .vm_variant = "beam.vanilla",
        .options = 1,
        .sizeof_ErlNifResourceTypeInit = @sizeOf(e.ErlNifResourceTypeInit),
        .min_erts = "erts-#{:erlang.system_info(:version)}"
      };

      export fn nif_init() *const e.ErlNifEntry{
        return &entry;
      }
      """ == %Module{nifs: [%Nif{name: :foo, arity: 0}],
                     resources: [%Resource{name: :bar, cleanup: :baz}],
                     file: "foo.exs",
                     module: Foo,
                     otp_app: :zigler}
      |> Code.footer
      |> IO.iodata_to_binary
    end

    test "works for multiple functions" do

      [major, minor] = Code.nif_major_minor()

      assert """
      // footer for Elixir.Baz in foo.exs:

      export var __exported_nifs__ = [_]e.ErlNifFunc{
        e.ErlNifFunc{
          .name = "foo",
          .arity = 0,
          .fptr = __foo_shim__,
          .flags = 0,
        },
        e.ErlNifFunc{
          .name = "bar",
          .arity = 1,
          .fptr = __bar_shim__,
          .flags = 0,
        },
      };

      const entry = e.ErlNifEntry{
        .major = #{major},
        .minor = #{minor},
        .name = "Elixir.Baz",
        .num_of_funcs = 2,
        .funcs = &(__exported_nifs__[0]),
        .load = beam.blank_load,
        .reload = beam.blank_load,     // currently unsupported
        .upgrade = beam.blank_upgrade, // currently unsupported
        .unload = beam.blank_unload,   // currently unsupported
        .vm_variant = "beam.vanilla",
        .options = 1,
        .sizeof_ErlNifResourceTypeInit = @sizeOf(e.ErlNifResourceTypeInit),
        .min_erts = "erts-#{:erlang.system_info(:version)}"
      };

      export fn nif_init() *const e.ErlNifEntry{
        return &entry;
      }
      """ == %Module{nifs: [
               %Nif{name: :foo, arity: 0},
               %Nif{name: :bar, arity: 1}],
             file: "foo.exs", module: Baz, otp_app: :zigler}
      |> Code.footer
      |> IO.iodata_to_binary
    end

    test "works for a threaded nif" do
      [major, minor] = Code.nif_major_minor()

      assert """
      // footer for Elixir.Baz in foo.exs:

      export var __exported_nifs__ = [_]e.ErlNifFunc{
        e.ErlNifFunc{
          .name = "__foo_launch__",
          .arity = 0,
          .fptr = __foo_launch__,
          .flags = 0,
        },
        e.ErlNifFunc{
          .name = "__foo_cleanup__",
          .arity = 1,
          .fptr = __foo_cleanup__,
          .flags = 0,
        },
      };

      const entry = e.ErlNifEntry{
        .major = #{major},
        .minor = #{minor},
        .name = "Elixir.Baz",
        .num_of_funcs = 2,
        .funcs = &(__exported_nifs__[0]),
        .load = beam.blank_load,
        .reload = beam.blank_load,     // currently unsupported
        .upgrade = beam.blank_upgrade, // currently unsupported
        .unload = beam.blank_unload,   // currently unsupported
        .vm_variant = "beam.vanilla",
        .options = 1,
        .sizeof_ErlNifResourceTypeInit = @sizeOf(e.ErlNifResourceTypeInit),
        .min_erts = "erts-#{:erlang.system_info(:version)}"
      };

      export fn nif_init() *const e.ErlNifEntry{
        return &entry;
      }
      """ == %Module{nifs: [%Nif{name: :foo, arity: 0, opts: [concurrency: :threaded]}],
             file: "foo.exs", module: Baz, otp_app: :zigler}
      |> Code.footer
      |> IO.iodata_to_binary
    end
  end
end
