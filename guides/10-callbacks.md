# Module Callbacks

The BEAM provides for a module to have several event-callbacks that are fired when a module is
created. Zigler labels these callbacks in the following way:

- [`on_load`](#on_load) - when the module is being loaded
- [`on_upgrade`](#on_upgrade) - when the module is being loaded to replace another module
- [`on_unload`](#on_unload) - when the module is being purged

These labeled are passed as options to the `use Zig` directive with the name of the function to be
used as the callback. All three are optional. You may use just the atom to stand a function with the
same name, e.g: `callbacks: [:on_load]` is shorthand for `callbacks: [on_load: :on_load]`

> ### Callbacks are pub {: .info}
>
> Be sure to make your callback functions `pub`. You do *not* need to add them to the module's
> `ignore` option.

> ### Context in callbacks {: .info}
>
> For all callbacks, the context is set with the following fields:

- `env`: the `e.ErlNifEnv` value passed to the callback function;
- `mode`: `.callback`
- `allocator`: `beam.allocator`

> Thus you should be able to use `beam.get`, `beam.make`, or `beam.send` with the appropriate context
> set without extra options.

## on_load

The on_load callback may have one of the following function signatures:

- `fn (?*?*T, U) void`: if the on_load function can never fail.
- `fn (?*?*T, U) !void`: if the on_load function can fail with an error. The module load integer will
  reflect the integer value of the error.------------------------
-
> #### Zig error integers {: .warning}
>
> Note that the integer representation of a zig error may change between compilations. Translating
>   this integer back to a meaningful value may be challenging.
- `fn (?*?*T, U) int`: the on_load function will be considered to fail if the integer value is not
  `0`.
- `fn (?*?*T, U) E`: for an `enum` type `E`, the on_load function will be considered to fail if the
  enum value is not zero. It's recommended that the enum type `E` be defined as so: `const E = enum{ ok = 0, ...};`
- `fn (beam.env, ?*?*anyopaque, e.ErlNifTerm) c_int`: this is a "raw" call that corresponds to the
  expected signature of a `upgrade` callback.

### on_load Callback types

`T` may be any type at all, which is considered the *private data* of the module. Any information
may be stored in a `*T`, and the double pointer is passed to the callback for the pointer to be
communicated to the VM. The `*T` supplied will be accessible via the `enif_priv_data` function

`U` must be a type that can be passed as the first argument of `beam.make`, and the value of `U`
will be set by the result of an `__on_load__/0` function defined in the module body. The result of
this function call will be passed to the `on_load`

> ### on_load and resources {: .info}
>
> The beam nif guide says that resources must be initialized in the `load` callback. The `on_load`
> callback must NOT initialize resources. Resource initialization is performed in a function that will
> wrap your `on_load` callback.

#### Example

```elixir
defmodule OnLoadExample do
  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler, callbacks: [on_load: :load_fn]

  defp __on_load__, do: 47

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  pub fn load_fn(private: ?*?*u32, number: u32) !void {
      const stored_pointer = try beam.allocator.create(u32);
      stored_pointer.* = number;
      private.?.* = stored_pointer;
  }

  pub fn get_private() u32 {
      const priv_ptr: ?*anyopaque = e.enif_priv_data(beam.context.env);
      const priv_ptr_u32: *u32 = @ptrCast(@alignCast(priv_ptr.?));
      return priv_ptr_u32.*;
  }
  """

  test "on_load stores value into private" do
    assert 47 = get_private()
  end
end
#module
```

## on_upgrade

The on_upgrade callback may have one of the following function signatures:

- `fn (?*?*T, ?*?*U, V) void`: if the on_load function can never fail.
- `fn (?*?*T, ?*?*U, V) !void`: if the on_load function can fail with an error. The module load
  integer will reflect the integer value of the error.------------------------
-
> #### Zig error integers {: .warning}
>
> Note that the integer representation of a zig error may change between compilations. Translating
>   this integer back to a meaningful value may be challenging.
- `fn (?*?*T, ?*?*U, V) int`: the on_load function will be considered to fail if the integer value is
  not `0`.
- `fn (?*?*T, ?*?*U, V) E`: for an `enum` type `E`, the on_load function will be considered to fail if
  the enum value is not zero. It's recommended that the enum type `E` be defined as so: `const E = enum{ ok = 0, ...};`
- `fn (beam.env, ?*?*anyopaque, ?*?*anyopaque, e.ErlNifTerm) c_int`: this is a "raw" call that
  corresponds to the expected signature of a `upgrade` callback.

### on_upgrade Callback types

`T` and `U` may be any type at all, which is considered the *private data* of the module. Any
information may be stored in these pointers, and the double pointer is passed to the callback for
the pointer to be communicated to the VM. The `*U` data supplied will be accessible via the
`enif_priv_data` function

`V` must be a type that can be passed as the first argument of `beam.make`, and the value of `V`
will be set by the result of an `__on_load__/0` function defined in the module body. The result of
this function call will be passed to the `on_load`

> ### on_upgrade and resources {: .info}
>
> The beam nif guide says that resources must be initialized in the `upgrade` callback. The
> `on_upgrade` callback must NOT initialize resources. This is performed in a function that will wrap
> your `on_upgrade` callback.

## on_unload

The on_unload callback may have one of the following function signatures:

- `fn (?*T) void`: if you would like your inbound private data to be typed.
- `fn (beam.env, ?*anyopaque) void`: this is a "raw" call that corresponds to the expected signature
  of a `upgrade` callback.
