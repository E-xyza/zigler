# Resources

Resources are datatypes which are managed by the BEAM reference-counted garbage collector. If you
are passing data between function calls, generally it is best practice to pass them as a resource
instead of as a pointer or a global variable.

For documentation of how resources work in general, see the erlang
[documentation](https://www.erlang.org/doc/man/erl_nif.html#resource_objects) on the topic. Note
that the conveniences in [beam.Resource](beam.html#Resource) generic type exist to make operations
type-safe.

> ### Resources are references {: .info}
>
> In managed language environments resources are passed as `t:reference/0` tokens.

Under the hood the BEAM allocates a chunk of memory on resource creation, and this memory is cleared
when the garbage collector is triggered. The BEAM also allows you to add in trigger hooks for when
this event happens, so custom cleanup can be tied into these GC events.

> ### Passing references between modules {: .warning}
>
> This is not currently supported by Zigler, but support is planned.

## Declaring a resource

In order to use a resource, you must do at a minimum three things:

1. declare the resource in your `use Zig` directive, under the `:resource` key. This is a list of
  atoms, which match the name of the resource type.
1. declare the wrapped type. This is the type of the data that is placed in the memory space of the
  resource.
1. use [`beam.Resource`](beam.html#Resource) to declare the resource type. note that this resource type
  must be `pub`.

```elixir
defmodule ResourceTest do
  use ExUnit.Case, async: true
  use Zig, 
    otp_app: :zigler,
    resources: [
      :StructResource,
      :PointerResource
    ]

  ~Z"""
  const beam = @import("beam");
  const root = @import("root");

  const MyStruct = struct {
      payload: u64
  };

  pub const StructResource = beam.Resource(MyStruct, root, .{});
  """
```

## Using resources in functions

```elixir
~Z"""
pub fn create_resource_term(number: u64) !beam.term {
    const res = try StructResource.create(.{.payload = number}, .{});
    return beam.make(res, .{});
}

pub fn retrieve_resource_term(term: beam.term) !u64 {
    const res = try beam.get(StructResource, term, .{});
    return res.unpack().payload;
}
"""

test "lifecyle operations through terms" do
  resource = create_resource_term(47)
  assert is_reference(resource)
  assert 47 = retrieve_resource_term(resource)
end
```

Resources can be marshalled into and out of [`beam.term`](beam.html#term) values using
[`beam.make`](beam.html#make) and [`beam.get`](beam.html#get) functions as with any other types.

In order to convert between the resource type and the wrapped type, you'll need to use the `create`
and `unpack` functions. Note that `create` is failable since under the hood it uses the BEAM
resource allocator.

### Direct marshalling

It's possible to directly return resources from a nif function and also pass them as parameters: The
nif marshalling functions will be able to detect these types and assign them correctly.

```elixir
~Z"""
pub fn create_resource_direct(number: u64) !StructResource {
    return StructResource.create(.{.payload = number}, .{});
}

pub fn retrieve_resource_direct(resource: StructResource) u64 {
    return resource.unpack().payload;
}
"""

test "direct lifecyle operations" do
  resource = create_resource_direct(47)
  assert is_reference(resource)
  assert 47 = retrieve_resource_direct(resource)
end

test "must be the correct type of reference" do
  assert_raise ArgumentError, """
  errors were found at the given arguments:

    * 1st argument: 
  
       expected: reference (for `beam.Resource(MyStruct, @import(\"root\"), .{...})`)
       got: `%{payload: 42}`
  """, fn ->
    retrieve_resource_direct(%{payload: 42})
  end

  non_resource_ref = make_ref()

  message = """
  errors were found at the given arguments:
  
    * 1st argument: 
  
       expected: reference (for `beam.Resource(MyStruct, @import(\"root\"), .{...})`)
       got: `#{inspect non_resource_ref}`
       note: the reference passed is not associated with a resource of the correct type
  """

  assert_raise ArgumentError, message, fn ->
    retrieve_resource_direct(non_resource_ref)
  end
end
```

> ### no coercion {: .warning}
>
> You can't pass a term of the same type as the wrapped type and use it within the function.

## Wrapping pointers and cleanup

In many cases you won't want to move large data structures into or out of the resource-allocated
memory space; this incurs a data copy cost. In that case, you might want to store a pointer in the
resource memory space.

In order to do properly clean up after this, you'll need to write a callback function and store it
in a struct namespace that gets associated with the resource type in using the `beam.Resource`
function `.Callbacks` option.

Note that the destructor can also be used in cases where other resources need to be cleaned up, for
example file descriptors.

The following functions are supported in the Callbacks, and are all optional.

- `dtor`: called when the GC collects the 
- `stop`: called on stop on behalf of `e.enif_select`
- `down`: called on resource down, on behalf of `e.enif_monitor_process`
- `dyncall`: called on dynamic resource call, on behalf of `enif_dynamic_resource_call`

```elixir
~Z"""
pub const PointerResource = beam.Resource(*MyStruct, root, .{.Callbacks = PointerResourceCallbacks});

pub const PointerResourceCallbacks = struct {
    pub fn dtor(s: **MyStruct) void {
        beam.allocator.destroy(s.*);
    }
};

pub fn create_pointer_resource(number: u64) !PointerResource {
    const new_struct = try beam.allocator.create(MyStruct);
    new_struct.payload = number;
    return PointerResource.create(new_struct, .{});
}

pub fn retrieve_pointer_resource(resource: PointerResource) u64 {
    return resource.unpack().*.payload;
}
"""

test "pointer-based lifecyle operations" do
  resource = create_pointer_resource(47)
  assert is_reference(resource)
  assert 47 = retrieve_pointer_resource(resource)
end
```

> ### pointer allocation strategy {: .warning}
>
> It is strongly recommended to use [`beam.allocator`](beam.html#allocator) for your pointer payload
> allocators, as [`beam.allocator`](beam.html#allocator) is undefined in the callback context.
>
> alternatively, if you do use your own managed allocator, you can pack a pointer to the allocator
> into your datastructure and use this pointer to clean up.

### Release and Keep

Resources are associated with `release` and `keep` functionality. These increment and decrement the
reference count on the resource which allows a nif to prevent the GC from destroying the memory and
calling the destructor. Normally the nif management functions select default `release` and `keep`
settings so that the functions do the most expected outcome - nif functions keep resources while
running and then release them when they're finished.

> ### release on creation {: .info}
>
> A Struct resource is normally released on creation. This can be disabled by passing `.release =
> false` into the options parameter of `beam.Resource(...).create(...)`

> ### keep on get {: .info}
>
> By default, [`beam.get`](beam.html#get) will keep the resource when the internal term retrieved.
> This can be disabled by passing `.keep = false` into the options parameter of `beam.get`

> ### get without keeping {: .warning}
>
> for wrapped datatypes that require a cleanup step (e.g. pointers) it is not recommended to get
> without keeping, as there could be a race condition where dereferencing the pointer occurs after
> another nif running in a different OS thread has performed a cleanup against the same resource.

> ### release when function argument {: .info}
>
> If a function is passed a `beam.Resource(...)` type, it will release it at the end of the call. This
> can be disabled by setting `:noclean` flag in the function argument options. (see [nif
> options](4-nif_options.html#noclean))

These functions are provided in the resource type as `beam.Resource(...).release(...)` and
`beam.Resource(...).keep(...)` functions, respectively:

```elixir
~Z"""
pub fn release(resource: StructResource) void {
    resource.release();
}

pub fn keep(resource: StructResource) void {
    resource.keep();
}
"""
```
