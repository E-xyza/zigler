//! This struct contains adapters designed to facilitate interfacing the
//! BEAM's c-style helpers for NIFs with a more idiomatic Zig-style of
//! programming, for example, the use of slices instead of null-terminated
//! arrays as strings.
//!
//! This struct derives from `priv/beam/beam.zig`, and is provided to the
//! project as a package.  You may import it into any project zig code
//! using the following code:
//!
//! ```zig
//! const beam = @import("beam");
//! ```
//!
//! If there's something in the BEAM nif API you need which is not provided,
//! you can also import `erl_nif` package which provides direct access to the
//! equivalent calls from [`erl_nif.h`](https://www.erlang.org/doc/man/erl_nif.html)
//! This can be done with the following code:
//!
//! ```zig
//! const e = @import("erl_nif");
//! ```

const e = @import("erl_nif");
const std = @import("std");
const options = @import("options.zig");

/// <!-- ignore -->
pub inline fn ignore_when_sema() void {
    // utility function for semantic analysis
    //
    // causes code in the function beyond the point to be be
    // ignored during semantic analysis passes.
    if (e.is_sema) unreachable;
}

/// boilerplate functions for nif module initialization.  Contains:
///
/// - `blank_load`
/// - `blank_upgrade`
/// - `blank_unload`
///
/// which are no-op versions of these functions.
pub const loader = @import("loader.zig");

/// identical to [`?*e.ErlNifEnv`](https://www.erlang.org/doc/man/erl_nif.html#ErlNifEnv)
///
/// > #### env {: .info }
/// >
/// > `env` should be considered an opaque type that can be passed around without inspection of
/// > its contents.
pub const env = ?*e.ErlNifEnv;

/// identical to [`?*e.ErlNifPid`](https://www.erlang.org/doc/man/erl_nif.html#ErlNifPid)
pub const pid = e.ErlNifPid;

/// identical to [`?*e.ErlNifPort`](https://www.erlang.org/doc/man/erl_nif.html#ErlNifPort)
pub const port = e.ErlNifPort;

/// A zig enum equivalent to [`e.ErlNifTermType`](https://www.erlang.org/doc/man/erl_nif.html#enif_term_type)
///
/// retrievable from [`term`](#term) using the [`beam.term_type`](#term-term_type) method.
pub const TermType = enum(e.ErlNifTermType) { atom = e.ERL_NIF_TERM_TYPE_ATOM, bitstring = e.ERL_NIF_TERM_TYPE_BITSTRING, float = e.ERL_NIF_TERM_TYPE_FLOAT, fun = e.ERL_NIF_TERM_TYPE_FUN, integer = e.ERL_NIF_TERM_TYPE_INTEGER, list = e.ERL_NIF_TERM_TYPE_LIST, map = e.ERL_NIF_TERM_TYPE_MAP, pid = e.ERL_NIF_TERM_TYPE_PID, port = e.ERL_NIF_TERM_TYPE_PORT, ref = e.ERL_NIF_TERM_TYPE_REFERENCE, tuple = e.ERL_NIF_TERM_TYPE_TUPLE };

/// wrapped term.
///
/// [`e.ErlNifTerm`](https://www.erlang.org/doc/man/erl_nif.html#ERL_NIF_TERM)
/// is, under the hood, an integer type.  This is wrapped in a singleton struct
/// so that that semantic analysis can identify and distinguish between a
/// 'plain' integer and a term.
///
/// The Zig compiler will optimize this away, so there is no runtime cost to
/// passing this around versus `e.ErlNifTerm`, and the following conversion
/// operations are no-ops:
///
/// - To convert to a raw `e.ErlNifTerm`, access the `.v` field.
/// - To convert a raw `e.ErlNifTerm` to this term, use an anonymous struct:
///
///     ```zig
///     .{.v = erl_nif_term_value}
///     ```
///
/// ### term_type
///
/// the struct function `term_type` returns the [`TermType`](#termtype) of the
/// internal term.
///
/// ```zig
/// const t = beam.term.make(env, 47, .{});
/// const term_type = t.term_type(env); // -> .integer
/// ```
///
pub const term = if (e.is_sema) struct {
    v: e.ErlNifTerm,
    pub fn term_type(_: *const @This(), _: anytype) TermType {
        return .atom;
    }
} else packed struct {
    v: e.ErlNifTerm,

    /// equivalent of e.enif_term_type
    pub fn term_type(this: *const @This(), opts: anytype) TermType {
        const environment = options.env(opts);
        return @enumFromInt(e.enif_term_type(environment, this.v));
    }
};

///////////////////////////////////////////////////////////////////////////////
// term generics

const get_ = @import("get.zig");
const make_ = @import("make.zig");
const cleanup_ = @import("cleanup.zig");
const processes = @import("processes.zig");
const stacktrace = @import("stacktrace.zig");

// TODO: eliminate this from the public namespace and use Payload.build...
/// <!-- ignore -->
pub const payload = @import("payload.zig");

/// <!-- topic: Term Management; args: dest_type, source_term, options -->
/// converts BEAM [`term`](#term) dynamic types into static zig types
///
/// The arguments are as follows:
/// 1. destination type
/// 2. term to convert
/// 3. struct (usually passed as anonymous) of keyword options for additional features.
///   See [supported options](#get-supported-options)
///
/// See also [`make`](#make) for the reverse operation.
///
/// The following type classes (as passed as 1st argument) are supported by `get`:
///
/// ### integer
/// - unsigned and signed integers are supported
/// - all integer sizes from 0..64 bits are supported (including non-power-of-2
///   sizes)
/// - for sizes bigger than 64, are supported, but the passed term must be a
///   native-endian binary with size n*64 bits
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn get_integer_example(term: beam.term) !i32 {
///     const x: i32 = try beam.get(i32, term, .{});
///     return x + 1;
/// }
///
/// // NOTE: the size of the integer in the following function
/// pub fn get_big_integer_example(term: beam.term) !u65 {
///   const x: u65 = try beam.get(u65, term, .{});
///   return x + 1;
/// }
/// """
///
/// test "get small integer" do
///   assert 48 = get_integer_example(47)
/// end
///
/// test "get big integer" do
///   # note we must pass as a binary, with width 128 bits.
///   assert 0x1_0000_0000_0000_0000 =
///     get_big_integer_example(
///       <<0xffff_ffff_ffff_ffff :: native-unsigned-64, 0::64>>
///     )
/// end
/// ```
///
/// ### enum
/// - may be passed the integer value of the enum.
/// - may be passed the atom representation of the enum.
/// - zero- and one- item enum types are not currently supported
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// const EnumType = enum {foo, bar};
/// pub fn get_enum_example(term: beam.term) !EnumType {
///     return try beam.get(EnumType, term, .{});
/// }
/// """
///
/// test "get integer enum" do
///   assert :foo = get_enum_example(0)
/// end
///
/// test "get atom enum" do
///   assert :foo = get_enum_example(:foo)
/// end
/// ```
///
/// ### float
/// - supports `f16`, `f32`, and `f64` types.
/// - may be passed a BEAM `t:float/0` term
/// - atoms `:infinity`, `:neg_infinity`, `:NaN` are also supported
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// const NanError = error {nanerror};
///
/// pub fn get_float_example(term: beam.term) !f32 {
///     const x: f32 = try beam.get(f32, term, .{});
///     if (!std.math.isNan(x)) {
///          return x + 1.0;
///     } else {
///          return error.nanerror;
///     }
/// }
/// """
///
/// test "get real float" do
///   assert 48.0 = get_float_example(47.0)
/// end
///
/// test "get infinite float" do
///   assert :infinity = get_float_example(:infinity)
///   assert :neg_infinity = get_float_example(:neg_infinity)
/// end
///
/// test "get nan float" do
///   assert_raise ErlangError, fn -> get_float_example(:NaN) end
/// end
///
/// ```
///
/// ### struct
/// - may be passed `t:map/0` with `t:atom/0` keys and values of the appropriate type
/// - may be passed a `t:keyword/0` list with `t:atom/0` keys and values of the
///   appropriate type.
/// - inner values are recursively marshalled to to the appropriate type.
/// - if the struct is `packed` or `extern`, supports binary data.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// const MathType = packed struct {
///     left: packed struct{ value: u32 },
///     right: packed struct{ value: u32 },
///     op: enum(u8) {add, sub}
/// };
///
/// pub fn get_struct_example(term: beam.term) !u32 {
///    const operation = try beam.get(MathType, term, .{});
///    switch (operation.op) {
///       .add => return operation.left.value + operation.right.value,
///       .sub => return operation.left.value - operation.right.value,
///    }
/// }
/// """
///
/// test "get struct" do
///   assert 49 = get_struct_example(%{
///     op: :add,
///     left: %{value: 47},
///     right: %{value: 2}})
///
///   assert 42 = get_struct_example(%{
///     op: :sub,
///     left: %{value: 47},
///     right: %{value: 5}})
/// end
///
/// test "get struct as packed binary" do
///   assert 49 = get_struct_example(<<47::native-32, 2::native-32, 0::8, 0::56>>)
/// end
/// ```
///
/// > #### packed and extern structs {: .warning }
/// >
/// > if you are doing binary encoding of these values, be mindful of native-endianness
/// > of the results and the need for padding to conform to alignment requirements.
/// >
/// > note that the C ABI specification may reorder your values, whereas a packed struct
/// > may have wider than expected alignment.
///
/// ### bool
/// - supports `true` and `false` `t:boolean/0` terms only.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn get_bool_example(term: beam.term) !bool {
///    return try beam.get(bool, term, .{});
/// }
/// """
///
/// test "get bool" do
///   assert get_bool_example(true)
///   refute get_bool_example(false)
/// end
/// ```
///
/// ### array
/// - supports lists of terms that can be converted to the array's element type.
/// - note that arrays have compile-time known length.
/// - if the array's element is integers, floats, packed or extern structs,
///   or arrays that support binaries, then the array can be passed binary data.
/// - does not perform allocation
///     > ### Allocation warning {: .warning }
///     >
///     > as allocation is not performed, getting could be a very expensive operation.
///     > these values will be on the stack.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// const PackedValue = packed struct { v: u32 };
/// pub fn get_array_example(term: beam.term) !u32 {
///     const array = try beam.get([3]PackedValue, term, .{});
///     var sum: u32 = 0;
///     for (array) |item| { sum += item.v; }
///     return sum;
/// }
/// """
///
/// test "get array" do
///   assert 144 = get_array_example([%{v: 47}, %{v: 48}, %{v: 49}])
/// end
///
/// test "get array as binary" do
///   assert 144 = get_array_example(<<47::native-32, 48::native-32, 49::native-32>>)
/// end
/// ```
///
/// ### single-item pointer
/// - used if an struct or array must be allocated on the heap.
/// - allocates memory based on allocator provided in the options, otherwise
///   defaults to [`beam.allocator`](#allocator)
/// - data may be passed as if they were not a pointer.
/// - returns an error if the allocation fails.
///
/// > ### Allocation warning {: .warning }
/// >
/// > it is the caller's responsibility to free the memory allocated by this function,
/// > via the `allocator.destroy` function, not `allocator.free`
///
/// #### Example
///
/// ```elixir
/// # see previous example cell for type definitions
/// ~Z"""
/// pub fn get_struct_pointer_example(term: beam.term) !u32 {
///     const structptr = try beam.get(*PackedValue, term, .{});
///     defer beam.allocator.destroy(structptr);
///
///     return structptr.v;
/// }
///
/// pub fn get_array_pointer_example(term: beam.term) !u32 {
///    const arrayptr = try beam.get(*[3]PackedValue, term, .{});
///    defer beam.allocator.destroy(arrayptr);
///
///    var sum: u32 = 0;
///    for (arrayptr.*) |item| { sum += item.v; }
///    return sum;
/// }
/// """
///
/// test "get struct pointer" do
///   assert 47 = get_struct_pointer_example(%{v: 47})
/// end
///
/// test "get array pointer" do
///   assert 144 = get_array_pointer_example(<<47::native-32, 48::native-32, 49::native-32>>)
/// end
/// ```
///
/// ### slice
/// - allocates memory based on allocator provided in the options, otherwise
///   defaults to [`beam.allocator`](#allocator)
/// - note that slice carries a runtime length
/// - supports list of any type
/// - supports binary of any type that can be represented as a fixed size binary.
///
/// > ### Allocation warning {: .warning }
/// >
/// > it is the caller's responsibility to free the memory allocated by this function,
/// > via the `allocator.free` function, not `allocator.destroy`
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn get_slice_example(term: beam.term) !u32 {
///    const slice = try beam.get([]packed struct{ v: u32 }, term, .{});
///    defer beam.allocator.free(slice);
///    var sum: u32 = 0;
///    for (slice) |item| { sum += item.v; }
///    return sum;
/// }
/// """
///
/// test "get slice" do
///   assert 144 = get_slice_example([%{v: 47}, %{v: 48}, %{v: 49}])
/// end
///
/// test "get slice as binary" do
///   assert 144 = get_slice_example(<<47::native-32, 48::native-32, 49::native-32>>)
/// end
/// ```
///
/// ### many-item-pointer
///
/// - allocates memory based on allocator provided in the options, otherwise
///   defaults to [`beam.allocator`](#allocator)
/// - supports list of any type
/// - supports binary of any type that can be represented as a fixed size binary.
/// - the runtime length is not a part of this datastructure, you are
///   expected to keep track of it using some other mechanism.  Caller is responsible
///   for tracking the length.
///    > ### Length warning {: .warning }
///    >
///    > due to the fact that this datatype drops its length information, this
///    > datatype should be handled with extreme care.
/// - the allocated runtime length can be retrieved by passing a `*usize` parameter
///   as the `size` parameter to the options.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn get_many_item_pointer_example(term: beam.term) !u32 {
///   var size: usize = undefined;
///   const pointer = try beam.get([*]u32, term, .{.size = &size});
///   defer beam.allocator.free(pointer[0..size]);
///   var sum: u32 = 0;
///   for (0..size) |index| { sum += pointer[index]; }
///   return sum;
/// }
/// """
///
/// test "get many item pointer" do
///   assert 144 = get_many_item_pointer_example([47, 48, 49])
/// end
///
/// test "get many item pointer as binary" do
///   assert 144 = get_many_item_pointer_example(<<47::native-32, 48::native-32, 49::native-32>>)
/// end
/// ```
///
/// ### cpointer
///
/// - allocates memory based on allocator provided in the options, otherwise
///   defaults to [`beam.allocator`](#allocator)
/// - supports list of any type
/// - supports binary of any type that can be represented as a fixed size binary.
/// - the runtime length is not a part of this datastructure, you are
///   expected to keep track of it using some other mechanism
///
///     > ### Length warning {: .warning }
///     >
///     > due to the fact that this datatype drops its length information, this
///     > datatype should only be used where c interop is needed
/// - the allocated runtime length can be retrieved by passing a `*usize` parameter
///   as the `size` parameter to the options.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn get_c_pointer_example(term: beam.term) !u32 {
///   var size: usize = undefined;
///   const pointer = try beam.get([*c]u32, term, .{.size = &size});
///   defer beam.allocator.free(pointer[0..size]);
///   var sum: u32 = 0;
///   for (0..size) |index| { sum += pointer[index]; }
///   return sum;
/// }
/// """
///
/// test "get c pointer" do
///   assert 144 = get_c_pointer_example([47, 48, 49])
/// end
///
/// test "get c pointer as binary" do
///   assert 144 = get_c_pointer_example(<<47::native-32, 48::native-32, 49::native-32>>)
/// end
/// ```
///
/// ### optional
///
/// - accepts `t:atom/0` `nil` as well as whatever the child type is.
///
/// > ### nil vs null {: .warning }
/// >
/// > note that zig uses `null` as its nil value, but zigler only accepts atom `nil`
/// > as its null value.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn get_optional_example(term: beam.term) !u32 {
///     const x = try beam.get(?u32, term, .{});
///     if (x) |value| { return value + 1; } else { return 47; }
/// }
/// """
///
/// test "get optional" do
///   assert 48 = get_optional_example(47)
///   assert 47 = get_optional_example(nil)
/// end
/// ```
///
/// ### pid
///
/// - accepts `t:pid/0`
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn get_pid_example(term: beam.term) !void {
///     const pid = try beam.get(beam.pid, term, .{});
///     try beam.send(pid, .foo, .{});
/// }
/// """
///
/// test "get pid" do
///   assert :ok = get_pid_example(self())
///   assert_receive :foo
/// end
/// ```
///
/// ## Supported options
///
/// - `env`: required for `raw` nifs.  If not provided, defaults to the context's
///   environment.
/// - `allocator`: the allocator to use for allocations.  If not provided, defaults
///   to the context's environment.
/// - `error_info`: pointer to a [`term`](#term) that can be populated with error
///   information that gets propagated on failure to convert.  If omitted, the code
///   to produce these errors will get optimized out.
/// - `keep` (`bool`, only applies to references):
///   if `true` (default) the refcount will be increased on the term as a result of
///   performing the `get` operation.
/// - `size` (`* usize`, only applies to manypointer or `[*c]`):
/////   optional in-out parameter to retrieve the size of the slice
pub const get = get_.get;

/// <!-- topic: Term Management; args: value, options -->
/// converts static zig types into BEAM [`term`](#term) dynamic types
///
/// The arguments are as follows:
/// - [environment](#env)
/// - `value` to convert to term
/// - `options` struct (usually passed as anonymous) of keyword options for additional features.
///   See [supported options](#get-supported-options).  Note this struct must be
///   comptime-known.
///
/// See also [`get`](#get) for the reverse operation.
///
/// The following zig types are supported:
///
/// ### [`term`](#term)
/// - no conversion is performed
/// - this type is necessary for recursive make operations
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_term_example(term: beam.term) beam.term {
///     return beam.make(term, .{});
/// }
/// """
///
/// test "make term" do
///   assert 47 = make_term_example(47)
/// end
/// ```
///
/// ### `void`
/// - returns atom `:ok`
/// - supporting this type makes metaprogramming easier.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_void_example() beam.term {
///    const v: void = undefined;
///    return beam.make(v, .{});
/// }
/// """
/// test "make void" do
///   assert :ok = make_void_example()
/// end
/// ```
///
/// ### [`pid`](#pid)
/// - converted into a [`term`](#term) representing `t:pid/0`
///
//// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_pid_example(pid: beam.pid) beam.term {
///    return beam.make(pid, .{});
/// }
/// """
/// test "make pid" do
///   assert self() == make_pid_example(self())
/// end
/// ```
///
/// ### `std.builtin.StackTrace`
/// - special interface for returning stacktrace info to BEAM.
///
/// ### integers
/// - unsigned and signed integers supported
/// - all integer sizes from 0..64 bits supported (including non-power-of-2
///   sizes)
/// - returns a BEAM `t:integer/0` term
/// - for sizes bigger than 64, supported, but the passed term will be
///   converted into a binary term bearing the integer encoded with
///   native endianness.
/// - comptime integers supported
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_integer_example(integer: u32) beam.term {
///   return beam.make(integer + 1, .{});
/// }
///
/// pub fn make_big_integer_example(integer: u65) beam.term {
///   return beam.make(integer + 1, .{});
/// }
///
/// pub fn make_comptime_integer_example() beam.term {
///   return beam.make(47, .{});
/// }
/// """
///
/// test "make integer" do
///   assert 48 = make_integer_example(47)
/// end
///
/// test "make big integer" do
///   assert <<0::64, 1::64-native>> = make_big_integer_example(0xFFFF_FFFF_FFFF_FFFF)
/// end
///
/// test "make comptime integer" do
///   assert 47 = make_comptime_integer_example()
/// end
/// ```
///
/// ### floats
/// - supports `f16`, `f32`, and `f64` types.
/// - supports comptime float type.
/// - returns a BEAM `t:float/0` term
/// - may also return one of the `t:atom/0` type
///   `:infinity`, `:neg_infinity`, `:NaN`
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_float_example(float: f32) beam.term {
///   return beam.make(float + 1.0, .{});
/// }
///
/// pub fn make_comptime_float_example() beam.term {
///   return beam.make(47.0, .{});
/// }
/// """
///
/// test "make float" do
///   assert 48.0 = make_float_example(47.0)
///   assert :infinity = make_float_example(:infinity)
///   assert :neg_infinity = make_float_example(:neg_infinity)
///   assert :NaN = make_float_example(:NaN)
/// end
///
/// test "make comptime float" do
///   assert 47.0 = make_comptime_float_example()
/// end
/// ```
///
/// ### bool
/// - supports `bool` types.
/// - returns a BEAM `t:boolean/0` term
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_bool_example(value: bool) beam.term {
///   return beam.make(!value, .{});
/// }
/// """
///
/// test "make bool" do
///   assert make_bool_example(false)
///   refute make_bool_example(true)
/// end
/// ```
///
/// ### enum or error enum
/// - supports `enum` or `error` types.
/// - doesn't support zero or one-item enums.
/// - returns a BEAM `t:atom/0` term
/// - also supports enum literals.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// const MakeEnums = enum {foo, bar};
/// pub fn make_enum_example(value: MakeEnums) beam.term {
///     return beam.make(value, .{});
/// }
///
/// pub fn make_enum_as_int_example(value: MakeEnums) beam.term {
///     return beam.make(value, .{.as = .integer});
/// }
///
/// const MakeErrorSet = error { MakeEnumError };
/// pub fn make_error_example() beam.term {
///     return beam.make(error.MakeEnumError, .{});
/// }
///
/// pub fn make_enum_literal_example() beam.term {
///     return beam.make(.foobarbaz, .{});
/// }
/// """
///
/// test "make enum" do
///   assert :foo = make_enum_example(:foo)
///   assert 0 = make_enum_as_int_example(:foo)
/// end
///
/// test "make error" do
///   assert :MakeEnumError = make_error_example()
/// end
///
/// test "make enum literal" do
///   assert :foobarbaz = make_enum_literal_example()
/// end
/// ```
///
/// > ### Enum literals {: .info }
/// >
/// > Enum literals are especially useful for returning atoms,
/// > such as `:ok` or `:error`.  Note that `error` is a reserved
/// > word in zig, so you will need to use `.@"error"` to generate
/// > the corresponding atom.  See also [`make_error_atom`](#make_error_atom)
///
/// ### optionals or null
/// - supports any child type supported by [`make`](#make)
/// - returns the `t:atom/0` type `nil` or the child type
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_null_example() beam.term {
///     return beam.make(null, .{});
/// }
///
/// pub fn make_optional_example(value: ?u32) beam.term {
///     return beam.make(value, .{});
/// }
/// """
///
/// test "make null" do
///   assert is_nil(make_null_example())
/// end
///
/// test "make optional" do
///   assert is_nil(make_optional_example(nil))
///   assert 47 = make_optional_example(47)
/// end
/// ```
///
/// ### arrays
///
/// - supports arrays of any term that can be encoded using [`make`](#make)
/// - note that arrays have compile-time known length.
/// - outputs as a list of the encoded terms
/// - arrays of `u8` default to outputting binary, this is the only exception
///   to the above rule.
/// - if the array's element is integers, floats, packed or extern structs,
///   or arrays that support binaries, then the array can be output as binary
///   data, by setting `as` option to `.binary`
/// - if the array's element is u8 and you would prefer outputting as a list,
///   setting `as` option to `.list` will do this.
/// - to specify the internal encoding of the array, pass to `as` a struct
///   with the field `list`; the value of the list field should be the encoding
///   of the internal terms.
///
/// #### Examples
///
/// ```elixir
/// ~Z"""
/// pub fn make_array_example() beam.term {
///     const array = [_]u32{47, 48, 49};
///     return beam.make(array, .{});
/// }
///
/// pub fn make_array_u8_example() beam.term {
///     const array = "foo";
///     return beam.make(array, .{});
/// }
///
/// pub fn make_array_binary_example() beam.term {
///     const array = [_]u32{47, 48, 49};
///     return beam.make(array, .{.as = .binary});
/// }
///
/// pub fn make_array_u8_list_example() beam.term {
///     const array = "foo";
///     return beam.make(array, .{.as = .list});
/// }
///
/// pub fn make_array_internal_encoding_example() beam.term {
///     const aoa = [_][2]u32{[2]u32{47, 48}, [2]u32{49, 50}};
///     return beam.make(aoa, .{.as = .{.list = .binary}});
/// }
/// """
///
/// test "make u8 array" do
///   assert "foo" = make_array_u8_example()
///   assert [102, 111, 111] = make_array_u8_list_example()
/// end
///
/// test "make u32 array" do
///   assert [47, 48, 49] = make_array_example()
///   assert <<47::native-32, 48::native-32, 49::native-32>> = make_array_binary_example()
/// end
///
/// test "make array with internal encoding" do
///   assert [
///     <<47::native-32, 48::native-32>>,
///     <<49::native-32, 50::native-32>>] = make_array_internal_encoding_example()
/// end
/// ```
///
/// ### structs
///
/// - supports structs with fields of any term that can be encoded using [`make`](#make)
/// - outputs as a `t:map/0` with atom keys and the encoded terms as values
/// - for `packed` or `extern` structs, supports binary data by setting `as`
///   option to `.binary`.  `extern` structs default to map encoding, `packed` structs
///   default to binary encoding.  All structs can be forced to `map` encoding by
///   passing setting `.as = .map`.
/// - encoding options can be specified by assigning `as` a struct with the
///   field `map`; the value of the map field should be a struct with keys matching
///   the struct fields and values being the encoding options for those fields.
///   It's legal to omit a field from this specification, in which case the encoding
///   will be `.default`.
/// - supports anonymous structs
/// - supports direct output as elixir structs using the `struct` option
///
/// #### Examples
///
/// ```elixir
/// ~Z"""
/// pub fn make_struct_example() beam.term {
///   return beam.make(.{.foo = 123, .bar = "bar", .baz = .baz}, .{});
/// }
///
/// pub fn make_elixir_struct_example() beam.term {
///   return beam.make(.{.first = 1, .last = 10, .step = 1}, .{.@"struct" = .@"Elixir.Range"});
/// }
///
/// const MakePacked = packed struct { value: u32 };
///
/// pub fn make_packed_struct() beam.term {
///   return beam.make(MakePacked{.value = 47}, .{});
/// }
///
/// pub fn make_struct_nested() beam.term {
///     const result = .{
///         .list = [2]u32{47, 48},
///         .binary = [2]u32{47, 48}
///     };
///     return beam.make(result, .{.as = .{.map = .{.binary = .binary}}});
/// }
/// """
///
/// test "make general struct" do
///   assert %{foo: 123, bar: "bar", baz: :baz} = make_struct_example()
/// end
///
/// test "make elixir struct" do
///   assert 1..10 = make_elixir_struct_example()
/// end
///
/// test "make packed struct" do
///   assert <<47::native-32>> = make_packed_struct()
/// end
///
/// test "make struct with nested format info" do
///   assert %{
///     binary: <<47::native-32, 48::native-32>>,
///     list: [47, 48]
///   } = make_struct_nested()
/// end
/// ```
///
/// ### tuples
///
/// - supports zig tuples with any term that can be encoded using [`make`](#make)
/// - outputs as a `t:tuple/0`.
/// - encoding will always proceed using `.default` encoding.  A scheme to specify
///   encoding options is planned in the future.
/// - note that the error atom should be encoded as `.@"error"`; you may also
///   use [`beam.make_error_atom(...)`](#make_error_atom)
///
/// #### Examples
///
/// ```elixir
/// ~Z"""
/// pub fn make_tuple_example() beam.term {
///    return beam.make(.{.ok, "foo", 47}, .{});
/// }
/// """
/// test "make tuple" do
///   assert {:ok, "foo", 47} = make_tuple_example()
/// end
/// ```
///
/// ### single-item-pointer
///
/// - these pointers are only supported for arrays and structs
/// - these are only supported because they are assumed to be pointers to
///   mutable data
/// - content will be dereferenced and encoded as if it were the child type
/// - `as` rules apply (see [arrays](#make-arrays) and [structs](#make-structs)).
///
/// #### Examples
///
/// ```elixir
/// ~Z"""
/// pub fn make_pointer_example() beam.term {
///     const array = [_]u32{47, 48, 49};
///     return beam.make(&array, .{});
/// }
/// """
///
/// test "make pointer" do
///   assert [47, 48, 49] = make_pointer_example()
/// end
/// ```
///
/// ### slice
///
/// - supports arrays of any term that can be encoded using [`make`](#make)
/// - note that arrays have compile-time known length.
/// - outputs as a list of the encoded terms
/// - slices of `u8` default to outputting binary, this is the only exception
///   to the above rule.
/// - if the slice's element is integers, floats, packed or extern structs,
///   or arrays that support binaries, then the slice can be output as binary
///   data, by setting `as` option to `.binary`
/// - `as` rules (see [arrays](#make-arrays)) apply.
///
/// #### Examples
///
/// ```elixir
/// ~Z"""
/// pub fn make_slice_example(slice: []u32) beam.term {
///    for (slice) |*item| { item.* += 1; }
///    return beam.make(slice, .{});
/// }
///
/// pub fn make_slice_binary_example(slice: []u32) beam.term {
///   return beam.make(slice, .{.as = .binary});
/// }
/// """
///
/// test "make slice" do
///   assert [48, 49, 50] = make_slice_example([47, 48, 49])
/// end
///
/// test "make slice as binary" do
///   assert <<47::native-32, 48::native-32, 49::native-32>> = make_slice_binary_example([47, 48, 49])
/// end
/// ```
///
/// ### many-item-pointer
///
/// - only supports [*:0]u8 and [*:null]?Pointer.
/// - `as` rules (see [arrays](#make-arrays)) apply.
///
/// #### Examples
/// ```elixir
/// ~Z"""
/// pub fn make_many_item_pointer_example(pointer: []u8) beam.term {
///   pointer[5] = 0;
///   const truncated: [*:0]u8 = @ptrCast(pointer.ptr);
///   return beam.make(truncated, .{});
/// }
/// """
/// test "make many item pointer" do
///   assert "hello" = make_many_item_pointer_example("hello world")
/// end
/// ```
///
/// ### cpointer
///
/// - only supported if the pointer has child type `u8` or pointer.
/// - in the case of `u8` interprets it as `[*:0]u8`.
/// - in the case of `Pointer` interprets it as `[*:null]?Pointer`.
/// - no other types are supported.
/// - note that the content will be interpreted as the pointer type,
///   so rules on pointers (see [single-item-pointers](#make-single-item-pointer)))
/// - `as` rules (see [arrays](#make-arrays)) apply.
///
/// #### Examples
///
/// ```elixir
/// ~Z"""
/// pub fn make_c_pointer_example(pointer: []u8) beam.term {
///   pointer[5] = 0;
///   const truncated: [*c]u8 = @ptrCast(pointer.ptr);
///   return beam.make(truncated, .{});
/// }
/// """
///
/// test "make c pointer" do
///  assert "hello" = make_c_pointer_example("hello world")
/// end
/// ```
pub const make = make_.make;

// special getters

/// <!-- topic: Term Management; args: source_list, options -->
///
/// This is a thin wrapper over [`e.enif_get_list_cell`](https://www.erlang.org/doc/man/erl_nif.html#enif_get_list_cell).
///
/// See also [`make_list_cell`](#make_list_cell) for the reverse operation.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn get_list_cell_example(term: beam.term) !i32 {
///     const x, const y = try beam.get_list_cell(term, .{});
///     return try beam.get(i32, x, .{}) + try beam.get(i32, y, .{});
/// }
/// """
///
/// test "get list_cell " do
///   assert 47 = get_list_cell_example([40 | 7])
/// end
pub const get_list_cell = get_.get_list_cell;

// special makers

/// <!-- topic: Term Management; args: string -->
/// turns a `[]const u8` into a the corresponding `t:atom/0` term.
///
/// returns a raised `ArgumentError` if the length of the string exceeds the
/// vm atom size limit (255 bytes)
///
/// This is a thin wrapper over [`e.enif_make_atom_len`](https://www.erlang.org/doc/man/erl_nif.html#enif_make_atom_len).
///
/// > ### Atom size limit {: .warning }
/// >
/// > The BEAM VM has a limit of 255 bytes for atom size.  This function does not protect against this
/// > and the API may change to accomodate this in the future.
///
/// > ### Atom table exhaustion {: .warning }
/// >
/// > The BEAM VM has a limit of 1_048_576 atoms.  This function does not provide any protection against
/// > atom table exhaustion.  A future version will protect against this case.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_into_atom_example() beam.term {
///    return beam.make_into_atom("foo", .{});
/// }
/// """
///
/// test "make into atom" do
///  assert :foo = make_into_atom_example()
/// end
/// ```
pub const make_into_atom = make_.make_into_atom;

/// <!-- topic: Term Management -->
/// returns the empty list term `[]`.
///
/// This is a thin wrapper over [`e.enif_make_empty_list`](https://www.erlang.org/doc/man/erl_nif.html#enif_make_empty_list).
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_empty_list_example() beam.term {
///   return beam.make_empty_list(.{});
/// }
/// """
///
/// test "make empty list" do
///   assert [] = make_empty_list_example()
/// end
/// ```
pub const make_empty_list = make_.make_empty_list;

/// <!-- topic: Term Management; args: head, tail, opts -->
/// performs a list cons operation for `head` and `tail` variables
///
/// This is a thin wrapper over [`e.enif_make_list_cell`](https://www.erlang.org/doc/man/erl_nif.html#enif_make_list_cell).
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_list_cell_example() beam.term {
///    return beam.make_list_cell(47, beam.make_empty_list(.{}), .{});
/// }
/// """
///
/// test "make list cell" do
///   assert [47] = make_list_cell_example()
/// end
/// ```
pub const make_list_cell = make_.make_list_cell;

/// <!-- topic: Term Management -->
/// shortcut for `make(.@"error", .{})`
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_error_atom_example() beam.term {
///   return beam.make_error_atom(.{});
/// }
/// """
///
/// test "make error atom" do
///   assert :error = make_error_atom_example()
/// end
/// ```
pub const make_error_atom = make_.make_error_atom;

/// <!-- topic: Term Management; args: value, options -->
/// shortcut for `make(env, .{.@"error", value}, options)`
pub const make_error_pair = make_.make_error_pair;

/// <!-- topic: Term Management; args: options -->
/// causes the VM to generate a new reference term
/// equivalent to `Kernel.make_ref/0`
///
/// This is a thin wrapper over [`e.enif_make_ref`](https://www.erlang.org/doc/man/erl_nif.html#enif_make_ref).
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn make_ref_example() beam.term {
///     return beam.make_ref(.{});
/// }
/// """
///
/// test "make_ref" do
///   assert is_reference(make_ref_example())
/// end
/// ```
pub const make_ref = make_.make_ref;

/// <!-- topic: Term Management; args: _, options -->
///
/// converts a zig `std.builtin.StackTrace` into a special term
/// that is designed to be translated and concatenated onto a BEAM
/// stacktrace.
///
/// ### Example term:
///
/// ```
/// [
///   %{
///     source_location: %{
///        file_name: "/path/to/project/lib/my_app/.Elixir.MyApp.MyModule.zig",
///        line: 15,
///        column: 5
///     },
///     symbol_name: "my_fun",
///     compile_unit_name: "Elixir.MyApp.MyModule"
///   }
///   ...
/// ]
/// ```
pub const make_stacktrace = stacktrace.to_term;

/// <!-- topic: Term Management; args: options -->
/// returns a [`pid`](#pid) value that represents the current or
/// parent process.
///
/// equivalent to `Kernel.self/0`
///
/// > #### scope {: .info }
/// >
/// > This function succeeds in all [contexts](#context), except for
/// > callback contexts.  For threaded processes, it will return the
/// > process that spawned the thread, whether or not that process is
/// > still alive.
///
/// Example:
///
/// ```elixir
/// ~Z"""
/// pub fn self_example() !beam.pid {
///   return try beam.self(.{});
/// }
/// """
///
/// test "self" do
///   assert self() == self_example()
/// end
/// ```
pub const self = processes.self;

/// <!-- topic: Term Management; args: _, data, options -->
/// sends `data` (as a term) to a target process' mailbox.
///
/// equivalent to `Kernel.send/2`
///
/// This function is a context-aware wrapper over
/// [`e.enif_send`](https://www.erlang.org/doc/man/erl_nif.html#enif_send).
/// that also formats the message term using [`make`](#make)
///
/// Note that `send` is designed so that you can switch concurrency modes
/// without having to change your code.
///
/// ### Options
///
/// - `clear` (boolean): whether the environment should be cleared after
///   sending the message.  Defaults to `true`.  See information below.
/// - `persist` (tuple of `beam.term`).  Persists the terms into the new
///   environment (see [`clear_env`](#clear_env)).  It is not an error to
///   pass `persist` in a process-bound context, though that will no-op.
/// - `as`: any formatting options to be passed when making the term
///   to be sent.
///
/// ### Return value
///
/// If there are no persisted values, `send` returns `void`.  Otherwise
/// it returns a tuple that corresponds to the tuple of persisted values,
/// with new `beam.term` values.
///
/// > ### send from raw nifs or non-process-bound threads {: .warning }
/// >
/// > This function has undefined behaviour when called from `raw` nifs or
/// > calls that are not process-bound (e.g. if a new thread is started
/// > from a posix call that is not managed by zigler)
/// >
/// > in the case of raw nifs, use `e.enif_send` directly instead.
/// >
/// > in the case of non-process-bound threads or raw calls, if you use
/// > `independent_context` to initialize the environment, you can use
/// > `send` as normal.
/// >
/// > The safety characteristics of raw and non-process-bound sending may
/// > be subject to change in future releases of Zigler.
///
/// > ### clearing your environment {: .info }
/// >
/// > normally when calling `e.enif_send` you would need to call `e.enif_clear_env`
/// > to recycle the environment after sending.  You do not need to do this
/// > for this function; it gets called automatically.
/// >
/// > If you are certain that the send operation is the last operation in your
/// > function call, you may call the function as `send(data, .{.clear = false})`
/// > and the clear_env function will not be called.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn send_example() !void {
///     const self = try beam.self(.{});
///     try beam.send(self, .{.foo, "bar", 47}, .{});
/// }
/// """
///
/// test "send" do
///   send_example()
///   assert_receive {:foo, "bar", 47}
/// end
/// ```
pub const send = processes.send;

// interfacing with functions

/// creates a tuple type that corresponds to the call signature of passed
/// function.
///
/// Using this tuple type, it is possible to call the function using
/// the [`@call`](https://ziglang.org/documentation/0.10.1/#call) builtin.
/// this is how calling functions can be easily made generic over multiple
/// concurrency types.
pub const Payload = payload.Payload;

/// <!-- topic: Term Management; args: value, options -->
///
/// generic cleanup function that can be used to cleanup values that have
/// been created by [`get`](#get).
///
/// The second parameter is an `options` parameters, which should be passed a
/// struct (possibly anonymous) with the following fields:
///
/// - `allocator`: which allocator should be used to clean up allocations.
///   optional, defaults to the threadlocal [`beam.context.allocator`](#context) value
/// - `size`: if the value does not have a defined size, for example a `Many`
///   pointer, you must provide this value.
///
/// noops on terms that do not require cleanup.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// const CleanupErrors = error{leaked};
///
/// pub fn cleanup_example(term: beam.term) !i32 {
///     var gpa = beam.make_debug_allocator_instance();
///     const allocator = gpa.allocator();
///     const slice = try beam.get([]i32, term, .{.allocator = allocator});
///     const number = slice[0];
///     beam.cleanup(slice, .{.allocator = allocator});
///     if (gpa.detectLeaks()) return error.leaked;
///     return number + 1;
/// }
/// """
///
/// test "cleanup" do
///   assert 48 = cleanup_example([47])
/// end
/// ```
///
pub const cleanup = cleanup_.cleanup;

// comparisons

/// result type for [`compare`](#compare)
///
/// these atoms are used to conform to Elixir's Compare interface
/// see: https://hexdocs.pm/elixir/1.13/Enum.html#sort/2-sorting-structs
pub const Compared = enum { lt, eq, gt };

/// <!-- topic: Term Management -->
/// compares two terms, following Elixir's compare interface.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn compare_example(lhs: beam.term, rhs: beam.term) beam.Compared {
///     return beam.compare(lhs, rhs);
/// }
/// """
///
/// test "beam.compare" do
///   assert :gt == compare_example(:infinity, 47)
///   assert :eq == compare_example(self(), self())
///   assert :lt == compare_example(:atom, "string")
/// end
/// ```
pub fn compare(lhs: term, rhs: term) Compared {
    const compared = e.enif_compare(lhs.v, rhs.v);

    if (compared == 0) return .eq;
    if (compared < 0) return .lt;
    if (compared > 0) return .gt;
    unreachable;
}

//////////////////////////////////////////////////////////////////////////////
// binaries

const binaries = @import("binaries.zig");

/// <!-- topic: Term Management -->
///
/// converts a [`e.ErlNifBinary`](https://www.erlang.org/doc/man/erl_nif.html#ErlNifBinary)
/// to `[]const u8`.
///
/// Does not perform allocations or copies
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn binary_to_slice_example(term: beam.term) u8 {
///   const e = @import("erl_nif");
///   var binary: e.ErlNifBinary = undefined;
///   _ = e.enif_inspect_binary(beam.context.env, term.v, &binary);
///   const slice = beam.binary_to_slice(binary);
///   return slice[2];
/// }
/// """
///
/// test "binary_to_slice" do
///   assert ?o = binary_to_slice_example("foo")
/// end
/// ```
///
/// > #### binary data {: .warning }
/// >
/// > This points to the data location of the binary, which might either be
/// > in the shared binary heap, or it might be in the process heap (for small
/// > binaries). This should be considered read-only, attempts to sneakily
/// > modify these data will have undefined effects, possibly including broken
/// > comparison operations.
pub const binary_to_slice = binaries.binary_to_slice;

/// <!-- topic: Term Management; args: _, binary -->
///
/// converts a `t:term/0` to a [`e.ErlNifBinary`](https://www.erlang.org/doc/man/erl_nif.html#ErlNifBinary)
/// using erlang term format serialization.
///
/// This is a thin wrapper over [`e.enif_term_to_binary`](https://www.erlang.org/doc/man/erl_nif.html#enif_term_to_binary).
///
/// returns `error.OutOfMemory` if the allocation fails.
pub const term_to_binary = binaries.term_to_binary;

/// <!-- topic: Term Management; args: encoded_term, options -->
///
/// converts a `[]u8` to a `t:term/0`.  The binary must be encoded using erlang term format.
///
/// This is a thin wrapper over [`e.enif_binary_to_term`](https://www.erlang.org/doc/man/erl_nif.html#enif_binary_to_term).
///
/// #### Example
/// ```elixir
/// ~Z"""
/// pub fn binary_to_term_example(encoded_number: []const u8) !u32 {
///   const term = try beam.binary_to_term(encoded_number, .{});
///   const number = try beam.get(u32, term, .{});
///   return number + 1;
/// }
/// """
///
/// test "binary_to_term" do
///   assert 48 = binary_to_term_example(:erlang.term_to_binary(47))
/// end
/// ```
pub const binary_to_term = binaries.binary_to_term;

/// <!-- topic: Term Management -->
///
/// marks a [`e.ErlNifBinary`](https://www.erlang.org/doc/man/erl_nif.html#ErlNifBinary) as qualified to be garbage
/// collected.
/// This is a thin wrapper over [`e.enif_release_binary`](https://www.erlang.org/doc/man/erl_nif.html#enif_release_binary).
pub const release_binary = binaries.release_binary;

///////////////////////////////////////////////////////////////////////////////
// options

const zigler_options = @import("zigler_options");

///////////////////////////////////////////////////////////////////////////////
// allocators

/// <!-- ignore -->
pub const allocator_ = @import("allocator.zig");

/// directly wraps the allocator functions into the `std.mem.Allocator`
/// interface.  This will only allocate memory at the machine word alignment.
/// if you need greater alignment, use `beam.allocator`
pub const raw_allocator = allocator_.raw_beam_allocator;

/// provides a BEAM allocator that can perform allocations with greater
/// alignment than the machine word.
///
/// > #### Memory performance {: .warning }
/// >
/// > This comes at the cost of some memory to store metadata and some
/// > performance on the allocation step.
pub const allocator = allocator_.beam_allocator;

/// a function which returns a new debug allocator instance.
pub const make_debug_allocator_instance = allocator_.make_debug_allocator_instance;

/// implements `std.mem.Allocator` using the `std.mem.DebugAllocator`
/// factory, backed by `beam.wide_alignment_allocator`.
pub const debug_allocator = allocator_.debug_allocator;

///////////////////////////////////////////////////////////////////////////////
// resources

const resource = @import("resource.zig");
pub const Resource = resource.Resource;

/// identical to `e.ErlNifEvent`.  This is an event datatype that the BEAM
/// documentation does not describe.
pub const event = e.ErlNifEvent;

/// identical to `e.ErlNifMonitor`.  This is a monitor datatype that the BEAM
/// documentation does not describe.
pub const monitor = e.ErlNifMonitor;

///////////////////////////////////////////////////////////////////////////////
// env management

/// <!-- topic: Env -->
/// Synonym for [`e.enif_alloc_env`](https://www.erlang.org/doc/man/erl_nif.html#enif_alloc_env)
///
/// generally, zigler takes care of environments for you, but there may be cases
/// where you have to manually allocate an environment.  These are *process
/// independent environments* which manage their own heaps.
///
/// use your own process independent environment, most `beam` functions can take a
/// `.env` option which lets you specify the environment; these functions generally
/// default to the context's environment (`beam.context.env`).
///
/// > #### pair with free_env {: .warning }
/// >
/// > all calls to `alloc_env/0` should be balanced with a call to `free_env/1`.
///
/// ```elixir
/// ~Z"""
/// pub fn alloc_env_example(pid: beam.pid) !void {
///     const env = beam.alloc_env();
///     defer beam.free_env(env);
///
///     try beam.send(pid, .{.ok, 47}, .{.env = env});
/// }
/// """
///
/// test "alloc_env_example" do
///   alloc_env_example(self())
///   assert_receive {:ok, 47}
/// end
/// ```
pub const alloc_env = e.enif_alloc_env;

/// <!-- topic: Env -->
/// Synonym for [`e.enif_free_env`](https://www.erlang.org/doc/man/erl_nif.html#enif_free_env)
///
/// See `alloc_env/0` for more information.
///
/// > #### context env warning {: .warning }
/// > unless you have created an independent context using `independent_context/1`,
/// > do not use this function to free the `beam.context.env`, as unexpected
/// > results may occur.
pub const free_env = e.enif_free_env;

/// <!-- topic: Env -->
/// copies a term from one env to to another
pub fn copy(env_: env, term_: term) term {
    return .{ .v = e.enif_make_copy(env_, term_.v) };
}

///////////////////////////////////////////////////////////////////////////////
// CONTEXT

/// <!-- topic: Context -->
/// a tag identifying the type of context in which the nif is running
///
/// See [nif documentation](https://www.erlang.org/doc/man/erl_nif.html#lengthy_work)
/// for more detailed information about the concurrency strategies.
/// - `.synchronous`: the execution context of a synchronous nif
/// - `.threaded`: the execution context of a nif that runs in its own os
///   thread
/// - `.dirty`: the execution context of a nif that runs on a dirty
///   scheduler
/// - `.yielding`: the execution context of a nif that runs cooperatively with
///   the BEAM scheduler
/// - `.callback`: the execution context of module setup/teardown callbacks or
///   a resource destruction callback
/// - `.dirty_yield`: the execution context of a dirty nif whose parent process
///   has been terminated but the nif is still running.
/// - `.independent`: the execution context of functions that are not associated
///   with nifs.  see [`independent_context`](#independent_context) for how to set this as the context.
///
/// See [`context`](#context) for the threadlocal variable that stores this.
///
/// > #### raw beam functions {: .warning }
/// >
/// > nifs called in `raw` mode are not assigned an execution context.
pub const ContextMode = enum { synchronous, dirty, callback, threaded, yielding, dirty_yield, independent };

/// <!-- topic: Context -->
///
/// a datastructure containing information that is local to the current call.
/// managed entrypoints into any given nif (any nif that isn't raw) will set
/// have access to the threadlocal `context/0` variable.
///
/// ### Fields
/// - `mode`: the concurrency mode of the nif
/// - `env`: the environment of the nif
/// - `allocator`: the allocator to use for allocations
pub const Context = struct { mode: ContextMode, env: env, allocator: std.mem.Allocator };

/// <!-- topic: Context -->
/// threadlocal variable that stores the execution context for the nif.
/// Execution of any managed (non-raw) nif is guaranteed to have access to
/// this variable, and the value of this variable will not be changed
/// across any reentries into the function (for example, for yielding nifs)
///
/// If you spawn a thread from your nif, you should copy anything you need
/// out of this variable.
///
/// See [`Context`](#executioncontext) for the list of fields.
///
/// > #### context starts undefined {: .warning }
/// >
/// > This threadlocal is set to `undefined` because of architectural differences:
/// > we cannot trust loaded dynamic libraries to properly set this on thread
/// > creation.
/// >
/// > `raw` function calls do not set `context`
pub threadlocal var context: Context = undefined;

/// <!-- topic: Context -->
/// convenience function to get the context's execution environment
pub fn get_env() env {
    return context.env;
}

/// <!-- ignore -->
const InitEnvOpts = struct { allocator: ?std.mem.Allocator = null };

/// <!-- topic: Env -->
/// creates an new `independent` environment and attaches it to the existing
/// context.  You may specify the `allocator` option to set the default
/// allocator for the context.  Defaults to `beam.allocator`.
///
/// ```elixir
/// ~Z"""
/// pub fn independent_context_example(pid: beam.pid) !void {
///    beam.independent_context(.{});
///    try beam.send(pid, beam.context.mode, .{});
///    beam.free_env(beam.context.env);
/// }
/// """
///
/// test "independent contexts" do
///   independent_context_example(self())
///   assert_receive :independent
/// end
/// ```
///
/// > ### free independent context envs {: .warning }
/// > you must ALWAYS free `beam.context.env` if you have created an independent
/// > context.
pub fn independent_context(opts: InitEnvOpts) void {
    context.env = e.enif_alloc_env();
    context.allocator = opts.allocator orelse allocator;
    context.mode = .independent;
}

// compile time check on the persistlist and substitutes `void` for the the return if the
// tuple is empty.
pub fn ClearEnvReturn(comptime T: type) type {
    const info = @typeInfo(T);
    if (info != .@"struct") @compileError("unsupported type for ClearEnvReturn, must be a tuple of `beam.term`");
    if (!info.@"struct".is_tuple) @compileError("unsupported type for ClearEnvReturn, must be a tuple of `beam.term`");
    const fields = info.@"struct".fields;

    // type assertion that it's a struct.
    inline for (fields) |field| {
        if (field.type != term) @compileError("unsupported type for CleanEnvReturn, must be a tuple of `beam.term`");
    }

    switch (fields.len) {
        0 => return void,
        1 => return term,
        else => return T,
    }
}

/// <!-- topic: Env -->
/// Performs [`e.enif_clear_env`](https://www.erlang.org/doc/man/erl_nif.html#enif_clear_env)
/// on the existing env in the context.  This function is generally called by `beam.send`,
/// but is provided if you need to manually trigger environment clearing.  The function is
/// also passed a tuple which is a list of terms that should be persisted into the new
/// environment.
///
/// This function will panic if the context's environment is not one known to be created
/// by `alloc_env`.
///
/// returns `void` if the persist list is empty; otherwise returns a tuple of the same size
/// as the persist list, contaning the terms that are persisted, in the same order.
///
/// ### Example
/// ```elixir
/// ~Z"""
/// pub fn clear_env_example(pid: beam.pid, term: beam.term) !void {
///    const env = beam.alloc_env();
///    defer beam.free_env(env);
///
///    const inner_term = beam.copy(env, term);
///    try beam.send(pid, .{.first, term}, .{.env = env, .clear = false});
///
///    // this function call is required since we did not clear our env
///    // after sending the term in the previous call.
///
///    const new_term = beam.clear_env(env, .{inner_term});
///    try beam.send(pid, .{.second, new_term}, .{.env = env, .clear = false});
///
///    return;
/// }
/// """
///
/// test "clear_env" do
///   clear_env_example(self(), 47)
///   assert_receive {:first, 47}
///   assert_receive {:second, 47}
/// end
/// ```
pub fn clear_env(env_: env, persist: anytype) ClearEnvReturn(@TypeOf(persist)) {
    const T = @TypeOf(persist);
    e.enif_clear_env(env_);
    if (@typeInfo(T).@"struct".fields.len == 1) {
        return .{ .v = e.enif_make_copy(env_, persist[0].v) };
    } else {
        var result: ClearEnvReturn(T) = undefined;
        inline for (persist, 0..) |p, index| {
            result[index] = .{ .v = e.enif_make_copy(context.env, p.v) };
        }
        return result;
    }
}

///////////////////////////////////////////////////////////////////////////////
// CONCURRENCY MODES

/// identical to `e.ErlNifTid`.  This is a thread id datatype that the BEAM.
/// documentation does not describe.
pub const tid = e.ErlNifTid;

const threads = @import("threads.zig");

/// <!-- args: function -->
/// Builds a datastructure that encapsulates information needed by a threaded
/// nif
///
/// this datastructure is intended to be wrapped in a resource, so that the
/// death of its parent process can be used to clean up the thread.
pub const Thread = threads.Thread;

/// <!-- args: callbacks -->
/// Builds a datastructure that defines callbacks for a threaded nif.
///
/// The argument is [`Thread`](#Thread) type.  The datastructure
/// produces a struct with a `dtor` callback that can be used to ensure
/// proper cleanup of the data in the thread.
///
/// these callbacks are called when the thread resource is destroyed.
/// Most importantly, this callback sets the threadlocal tracker in
/// that [`yield`](#yield) investigates to determine if the parent
/// process should be terminated.
///
/// see [`Resource`](#Resource) for more details on the callbacks.
pub const ThreadedCallbacks = threads.Callbacks;

const yield_ = @import("yield.zig");

/// <!-- topic: Concurrency -->
/// periodic check-in function for long-running nifs.
///
/// always returns `error.processterminated` if the calling
/// process has been terminated.
///
/// Note that the environment of the function may be invalid
/// so if you must send information back to the BEAM on a
/// process termination event, you should have a beam environment
/// allocated to communicate with the BEAM using [`send`](#send)
///
/// For yielding nifs (not implemented yet):
///
/// - relinquishes control to the BEAM scheduler.
/// - If the thread's parent process has been killed, returns
///   `error.processterminated`
/// - when control is returned from the scheduler, resumes
///   with no error.
/// - creates an async suspend point.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn yielding_example(pid: beam.pid) !void {
///     // for this to work, we must have an independent
///     // environment, as the process environment will have been
///     // destroyed when the beam.yield error return occurs.
///     const env = beam.alloc_env();
///     defer beam.free_env(env);
///
///     while (true) {
///        std.Thread.sleep(100000);
///        beam.yield() catch {
///           try beam.send(pid, .died, .{.env = env});
///           return;
///        };
///     }
/// }
/// """
///
/// test "yield" do
///   test_pid = self()
///   spawn_pid = spawn(fn -> yielding_example(test_pid) end)
///   Process.sleep(100)
///   Process.exit(spawn_pid, :kill)
///   assert_receive :died, 1000
/// end
/// ```
pub const yield = yield_.yield;

// wrappedresult: for yielding and threaded nifs we have to do something a bit
// different to wrap a zig error across the beam boundary.  This common utility
// type is used for that.

const WrappedResultTag = enum { ok, error_return_trace };

/// <!-- ignore -->
pub fn WrappedResult(comptime FunctionType: type) type {
    const NaiveReturnType = @typeInfo(FunctionType).@"fn".return_type.?;
    return switch (@typeInfo(NaiveReturnType)) {
        .error_union => |eu| union(WrappedResultTag) {
            ok: eu.payload,
            error_return_trace: term,
        },
        else => NaiveReturnType,
    };
}

///////////////////////////////////////////////////////////////////////////////
// ETC

/// <!-- topic: Exceptions -->
/// The equivalent of [`error`](https://www.erlang.org/doc/man/erlang.html#error-1)
/// in erlang.
///
/// > ### special-cased exception terms {: .info }
/// >
/// > N.B. certain erlang terms are caught and handled as special cases
/// > in elixir, see the following example:
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn raise_exception_example() beam.term {
///     return beam.raise_exception(.badarg, .{});
/// }
/// """
///
/// test "raise_exception" do
///   assert_raise ArgumentError, fn ->
///     raise_exception_example()
///   end
/// end
/// ```
pub fn raise_exception(reason: anytype, opts: anytype) term {
    return term{ .v = e.enif_raise_exception(options.env(opts), make(reason, opts).v) };
}

/// <!-- ignore -->
pub fn thread_not_running(err: anytype) bool {
    if (has_processterminated(@TypeOf(err))) {
        return (err == error.processterminated);
    } else return false;
}

fn has_processterminated(comptime T: type) bool {
    inline for (@typeInfo(T).error_set.?) |err| {
        if (std.mem.eql(u8, err.name, "processterminated")) return true;
    }
    return false;
}

/// <!-- topic: Exceptions -->
///
/// The equivalent of `Kernel.raise/1` from elixir.
///
/// - `module` should be the name of the module that represents the exception
/// - `data` should be a struct (possibly anonymous) that represents the Elixir
///   exception payload.
///
/// #### Example
///
/// ```elixir
/// ~Z"""
/// pub fn raise_example() beam.term {
///     return beam.raise_elixir_exception(
///         "CompileError",
///         .{.file = "foo", .line = 42, .description = "something went wrong"},
///         .{});
/// }
/// """
///
/// test "raise_elixir_execption" do
///   assert_raise CompileError, "foo:42: something went wrong", fn ->
///     raise_example()
///   end
/// end
/// ```
///
/// > #### Exception structs are not type checked {: .warning }
/// >
/// > Unlike code in elixir, the validity of the exception struct is not
/// > checked when using this function.
pub fn raise_elixir_exception(comptime module: []const u8, data: anytype, opts: anytype) term {
    if (@typeInfo(@TypeOf(data)) != .@"struct") {
        @compileError("elixir exceptions must be structs");
    }

    const env_ = options.env(opts);

    const name = comptime name: {
        break :name "Elixir." ++ module;
    };
    var exception: e.ErlNifTerm = undefined;
    const initial = make(data, .{});

    _ = e.enif_make_map_put(env_, initial.v, make_into_atom("__struct__", opts).v, make_into_atom(name, opts).v, &exception);
    _ = e.enif_make_map_put(env_, exception, make_into_atom("__exception__", opts).v, make(true, opts).v, &exception);

    return raise_exception(term{ .v = exception }, opts);
}

/// <!-- topic: Exceptions -->
/// Raises with a special error datatype that contains an term-encoded stacktrace
/// datastructure.  See also [`make_stacktrace`](#make_stacktrace)
///
/// This datastructure is designed to be concatenated onto the existing
/// stacktrace.  In order to concatenate this stacktrace onto your BEAM
/// exception, the function that wraps the nif must be able to catch the
/// error and append the zig error return trace to the existing stacktrace.
pub fn raise_with_error_return(err: anytype, maybe_return_trace: ?*std.builtin.StackTrace, opts: anytype) term {
    if (@import("builtin").os.tag == .windows) return raise_exception(.{ .@"error", err, null }, opts);

    return if (maybe_return_trace) |return_trace|
        raise_exception(.{ .@"error", err, return_trace }, opts)
    else
        raise_exception(.{ .@"error", err, null }, opts);
}

// unignore this on 0.15, if this is validated

/// <!-- ignore -->
pub const Mutex = @import("mutex.zig").Mutex;
