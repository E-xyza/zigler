# Global module options

## module callbacks

Sometimes it's necessary to execute nif code on module lifecycle events.  Zigler
supports binding function into these 

> ### pub and ignore {: .info }
> callback functions must be exported from the module nifs definition code as
> `pub` functions.
>
> Don't forget to include any callback functions in the [`ignore`](4-nif_options.html#ignore)
> list.

> ### callback api {: .info }
> the callback system api may get revised in the future to enable better features
> and make the priv data system typesafe.

### load

The `load` callback is triggered when the module is first loaded.  This
is useful for, for example, setting up global configuration for the
module, and saving data to be accessed using the [`enif_priv_data`](https://www.erlang.org/doc/man/erl_nif.html#enif_priv_data) function.

> ### load function type signature {: .info }
>
> The type signature of the load function is expected to be:
> 
> `fn (beam.env, [*c]?*anyopaque, beam.term)`
>
> The second term is a pointer to the location of the priv data.  the load 
> function can optionally set this pointer, which will enable access to
> that priv_data pointer via `enif_priv_data`.
>
> This API may change in the future to avoid the opaque c double pointer.

```elixir
defmodule ZiglerTest.LoadTest do
  use ExUnit.Case, async: true
  use Zig, 
    otp_app: :zigler, 
    callbacks: [
      on_load: :load_function,
    ],
    ignore: [:load_function]

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");
  const std = @import("std");

  var priv_data: u64 = undefined; 

  pub fn load_function(env: beam.env, priv_data_ptr: [*c]?*anyopaque, init_term: beam.term) c_int {
      priv_data = 47;
      priv_data_ptr.* = &priv_data;
      _ = init_term;
      _ = env;
      return 0;
  }

  pub fn get_priv_data(env: beam.env) u64 {
      const priv_data_ptr: *u64 = @ptrCast(@alignCast(e.enif_priv_data(env)));
      return priv_data_ptr.*;
  }

  pub fn loaded_value() u64 {
      return priv_data;
  }
  """

  test "module load" do
    assert 47 = loaded_value()
    assert 47 = get_priv_data()
  end

end
```

> ### load_nif data {: .error }
>
> The third term (`init_data`) is supplied on invocation of `:erlang.load_nif/2`.  
> Assigning this value is not supported in this version of zigler.

### upgrade

The `upgrade` callback is triggered when the module is upgraded from a
previous version of the module during a hot swap event.

> ### upgrade not supported {: .error }
>
> the upgrade callback is not supported in this version of zigler

### unload

The `unload` callback is triggered when the module is deregistered from
the BEAM module catalog.

> ### unload not supported {: .error }
>
> the unload callback is not supported in this version of zigler

## adding packages

It's possible to add zig files as packages using the `packages` 
keyword option.  The name of the package is the key, and the value 
is a tuple of the path to the zig file that acts as the package and
a list of dependencies for the package.  

### Example extra.zig

```zig
pub const value = 47;
```

```elixir
defmodule PackageFile do
  use ExUnit.Case, async: true
#  use Zig, 
#    otp_app: :zigler,
#    packages: [extra: {"test/_support/package/extra.zig", [:beam]}]
#
#  ~Z"""
#  const extra = @import("extra");
#
#  pub fn extra_value() u64 {
#    return extra.value;
#  }
#  """
#
  test "package file" #do
#    assert 47 = extra_value()
#  end
end
#module
```