# Collection Datatypes

## Returning array-like datatypes

For array-like datatypes, we saw in [Using Nifs](1-nifs.html#example-array-like-datatypes) 
how they can take both lists or binaries as inputs.  However, when returning
statically-typed data from zig, a choice needs to be made as to whether to
return lists or binaries.

### Arrays

Arrays are the simplest array-like datatype to return.

```elixir
#~Z"""
#pub fn return_array(input: f32) [3]f32 {
#  var result: [3]f32 = undefined;
#
#  // set each item in the array:
#  for (&result, 0..) |*item, index| {
#    item.* = input + @as(f32, @floatFromInt(index));
#  }
#  return result;
#}
#"""
#
#test "returning an array" do
#  assert [47.0, 48.0, 49.0] == return_array(47.0)
#end
```

### Slices

Slices can also be returned.  Note that in many cases, returning
a slice might need an [allocation strategy](3-allocators.html)

```elixir
#~Z"""
#pub fn return_slice(input: []f32) []f32 {
#  // set each item in the array:
#  for (input, 0..) |*item, index| {
#    item.* = item.* + @as(f32, @floatFromInt(index));
#  }
#  return input;
#}
#"""
#
#test "returning a slice" do
#  assert [47.0, 48.0, 49.0] == return_slice([47.0, 47.0, 47.0])
#end
```

> ### u8 array-likes output as binary {: .info }
>
> u8 array-like datatypes are marshalled into binary 
> by default instead of list.

```elixir
#~Z"""
#pub fn return_u8_array() [3]u8 {
#    const result: [3]u8 = .{97, 98, 99};
#    return result;
#}
#"""
#
#test "u8 datatypes are returned as binary" do
#  assert "abc" == return_u8_array()
#end 
```

### Selecting output type

It's also possible to return these collections as binaries,
however, in order to do so you will have to marshal manually.
For datatypes that are more than 1 byte, be aware that the endianness
of the resulting data is `native`.

```elixir
#~Z"""
#const beam = @import("beam");
#
#pub fn return_slice_binary(env: beam.env, input: []f32) beam.term {
#  // set each item in the array:
#  for (input, 0..) |*item, index| {
#    item.* = item.* + @as(f32, @floatFromInt(index));
#  }
#  return beam.make(env, input, .{.output_type = .binary});
#}
#"""
#
#test "returning a slice as binary" do
#  assert <<47.0 :: float-size(32)-native, 
#           48.0 :: float-size(32)-native,
#           49.0 :: float-size(32)-native>> == return_slice_binary([47.0, 47.0, 47.0])
#end
```

Conversely, for u8 array-like datatypes, selecting `.output_type = .list` will
result in outputting a charlist.

You can also automatically marshal as binary by using
[nif options](4-nif_options.html#binary-output)

### Full list of qualified array-like return types

The central challenge of exporting array-like data to the BEAM is that length
information may not be known.  In the case of arrays and slices, length 
is either comptime or runtime known.  For other data types, the scope of
datatypes accepted must be limited:

- array of any type (`[_]T`)
- single pointers to an array (`*[_]T`)
- slices (`[]T`)
- sentinel-terminated forms of the above (`[_:0]T` or `[:0]T`)
- sentinel-terminated many-item-pointer (`[*:0]T`)
- cpointer to u8 (`[*c]u8`).  This will assume the cpointer is null-terminated,
  if it can't be considered `[*:0]u8` the behaviour is undefined.
- cpointer to a pointer (`[*c]?*T`).  This will assume cpointer is 
  null-terminated, if it can't be considered `[*:null]?*T` then the
  behaviour is undefined.

## Passing and returning enums

Enums are collections of integer values that are given special identifier
status in the zig programming language.  At compile-time, it's possible
to get reflection on the string representation of those identifiers.  
Zigler thus is enabled to use enums as a representation of atoms coming 
from or going to the BEAM.

### Enums as atoms

Enums can be created by referring to them by the atom that corresponds
to their value:

```elixir
#~Z"""
#const EnumType = enum(u8) {
#  foo, 
#  bar = 47
#};
#
#pub fn flip(input: EnumType) EnumType {
#  return switch (input) {
#    .foo => .bar,
#    .bar => .foo
#  };
#}
#"""
#
#test "flipping enums" do
#  assert :bar = flip(:foo)
#  assert :foo = flip(:bar)
#end
```

### Enums as integers

Functions taking an integer type can also by passed the associated integer 
value in the place of the atom.

```elixir
#test "enums passed as integer" do
#  assert :foo = flip(47)
#end
```

### Enum literals

Enum literals can be converted to atoms using [`beam.make`](beam.html#make).

```elixir
#~Z"""
#pub fn make_literal(env: beam.env) beam.term {
#  return beam.make(env, .some_new_literal, .{});
#}
#"""
#
#test "enum literals" do
#  assert :some_new_literal = make_literal()
#end
```

This is especially useful for emitting `:ok` or `:error` tuples.

> ### error atom {: .info }
>
> `error` is a reserved word in Zig, so to create error atom you must use
> builtin syntax; the error enum literal is represented `.@"error"`.

## Passing and returning structs

### Structs are atom-keyed maps

Most structs are interpreted as atom-keyed maps.  Consider the following code:

```elixir
#~Z"""
#pub const Point2D = struct{ x: i32, y: i32 };
#
#pub fn reflect(input: Point2D) Point2D {
#  return .{.x = input.y, .y = input.x};
#}
#"""
#
#test "structs" do
#  assert %{x: 48, y: 47} == reflect(%{x: 47, y: 48})
#end
```

> ### Structs in parameters and returns {: .info }
>
> for a struct type to be used in parameters and returns,
> it must be exported as `pub` in the module interface

### Anonymous structs can be returned

It's possible to return anonymous structs as well, using 
[`beam.make`](beam.html#make).

```elixir
#~Z"""
#pub fn anonymous_struct(env: beam.env) beam.term {
#    return beam.make(env, .{.foo = .bar}, .{});
#}
#"""
#
#test "anonymous structs" do
#  assert %{foo: :bar} == anonymous_struct()
#end
```

### Zig tuples are structs.

Tuples in zig are structs (with hidden integer-valued keys).
Zigler takes advantage of this and allows you to construct
BEAM tuples using zig tuples, when passed to [`beam.make`](beam.html#make).

```elixir
#~Z"""
#pub fn tuple(env: beam.env) beam.term {
#    return beam.make(env, .{.ok, 47}, .{});
#}
#"""
#
#test "tuples" do
#  assert {:ok, 47} == tuple()
#end
```

### Packed or Extern structs.

Packed or Extern structs can be passed as binaries.

```elixir
#~Z"""
#pub const Packed = packed struct {x: u4, y: u4};
#
#pub const Extern = extern struct {x: u16, y: u16};
#
#pub fn diff_packed(value: Packed) u8 {
#    return value.x - value.y;
#}
#
#pub fn diff_extern(value: Extern) u16 {
#    return value.x - value.y;
#}
#"""
#
#test "packed and extern structs as struct" do
#  assert 2 = diff_packed(%{x: 7, y: 5})
#  assert 5 = diff_extern(%{x: 47, y: 42})
#end
#
#test "packed and extern structs as binary" do
#  assert 2 = diff_packed(<<0x57>>)
#  assert 5 = diff_extern(<<47::unsigned-size(16)-native, 42::unsigned-size(16)-native>>)
#end
```

> ### Packed And Extern Endianness {: .warning }
>
> Be careful about the endianness of packed and extern structs!

### Pointers to structs.

Pointers to structs can also be used to marshal data in and out.  This
is enabled under the assumption that you might want to use the struct
in a mutable fashion.

```elixir
#~Z"""
#pub fn swap_pointer(value: *Point2D) *Point2D {
#    const temp = value.x;
#    value.x = value.y;
#    value.y = temp;
#
#    return value;
#}
#"""
#
#test "pointer to structs" do
#  assert %{x: 47, y: 50} == swap_pointer(%{x: 50, y: 47})
#end
```

## Nested collections

The process of marshalling parameters and returns also works with 
nested array-like and struct data.

### Arraylike of arraylike

```elixir
#~Z"""
#pub fn array_of_array_sum(a_of_a: [][]u64) u64 {
#    var sum: u64 = 0;
#    for (a_of_a) |inner_array| {
#        for (inner_array) |value| {
#            sum += value;
#        }
#    }
#    return sum;
#}
#"""
#
#test "array of array" do
#  assert 21 = array_of_array_sum([[1, 2, 3], [4], [5, 6]])
#end
```

### Structs of structs

```elixir
#~Z"""
#pub const Arrow = struct {
#    head: Point2D,
#    tail: Point2D
#};
#
#pub fn reflect_reverse_arrow(arrow: Arrow) Arrow {
#    return .{
#      .head = reflect(arrow.tail), 
#      .tail = reflect(arrow.head)
#    };
#}
#"""
#
#test "structs of structs" do
#  assert %{
#    head: %{x: 1, y: 2},
#    tail: %{x: 3, y: 4}
#  } == reflect_reverse_arrow(%{
#    head: %{x: 4, y: 3},
#    tail: %{x: 2, y: 1}
#  })
#end
```

> #### Deep Argument Errors {: .info }
>
> Argument errors for deeply nested structs will help you understand
> where your arguments failed to serialize:

```elixir
# note: skipped because map key order is nondeterministic before 1.15
@tag [skip: Version.compare(System.version(), "1.15.0") == :lt]
#test "argument errors" do
#  assert_raise ArgumentError, """
#  errors were found at the given arguments:
#  
#    * 1st argument: 
#  
#       expected: map | keyword (for `Arrow`)
#       got: `%{head: %{x: 4, y: 3}, tail: %{x: 2, y: 1.0}}`
#       in field `:tail`:
#       | expected: map | keyword (for `Point2D`)
#       | got: `%{x: 2, y: 1.0}`
#       | in field `:y`:
#       | | expected: integer (for `i32`)
#       | | got: `1.0`
#  """, fn ->
#    reflect_reverse_arrow(%{
#      head: %{x: 4, y: 3},
#      tail: %{x: 2, y: 1.0}
#    })
#  end
#end
```

### Arraylike of structs

```elixir
#~Z"""
#pub fn sum_points(points: []Point2D) Point2D {
#    var result: Point2D = .{.x = 0, .y = 0};
#    for (points) |point| {
#        result.x += point.x;
#        result.y += point.y;
#    }
#    return result;
#}
#"""
#
#test "array of struct" do
#  assert %{x: 9, y: 12} = sum_points([%{x: 1, y: 2}, %{x: 3, y: 4}, %{x: 5, y: 6}])
#end
```

### Structs of arraylikes

```elixir
#~Z"""
#pub const PointSOA = struct{
#  x: []u16,
#  y: []u16
#};
#
#pub fn sum_point_soa(points: PointSOA) Point2D {
#    var result: Point2D = .{.x = 0, .y = 0};
#    for (points.x) |x| {
#        result.x += x;
#    }
#    for (points.y) |y| {
#        result.y += y;
#    }
#    return result;
#}
#"""
#
#test "struct of array" do
#  assert %{x: 9, y: 12} = sum_point_soa(%{x: [1, 3, 5], y: [2, 4, 6]})
#end
```

> ## Interaction with allocators {: .warning }
>
> if you directly return a datatype that was allocated, it won't be 
> properly cleaned up.  However, it can be properly cleaned up by
> manually deferring its cleanup after calling [`beam.make`](beam.html#make).
>
> Cleanup routines in [nif options](4-nif_options.html) will be introduced
> in a future release, which will enable protection from these sorts of leaks.
>
> For more on allocators, see [allocators](3-allocators.html)

```elixir
#~Z"""
#pub fn leaks() !*Point2D {
#    var point = try beam.allocator.create(Point2D);
#
#    point.x = 47;
#    point.y = 50;
#
#    return point;
#}
#
#pub fn no_leak(env: beam.env) !beam.term {
#    var point = try beam.allocator.create(Point2D);
#    defer beam.allocator.destroy(point);
#
#    point.x = 47;
#    point.y = 50;
#
#    return beam.make(env, point, .{});
#}
#"""
#
#test "both leaky and non-leaky struct returns work" do
#  assert %{x: 47, y: 50} == leaks()
#  assert %{x: 47, y: 50} == no_leak()
#end
```
