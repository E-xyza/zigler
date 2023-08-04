//! This struct contains adapters designed to facilitate interfacing the
//! BEAM's c-style helpers for NIFs with a more idiomatic Zig-style of
//! programming, for example, the use of slices instead of null-terminated
//! arrays as strings.
//!
//! This struct derives from `zig/beam/beam.zig`, and is provided to the
//! project as a package.  You may import it into any project zig code
//! using the following code:
//!
//! ```
//! const beam = @import("beam")
//! ```
//!
//! If there's something you need which is not provided, you can also
//! import `erl_nif` package which provides direct access to the
//! equivalent calls from [`erl_nif.h`](https://www.erlang.org/doc/man/erl_nif.html)

const e = @import("erl_nif.zig");
const std = @import("std");
const BeamMutex = @import("mutex.zig").BeamMutex;

/// `true` if code is being compiled during a semantic analysis pass
pub const is_sema = @import("sema").is_sema;

/// <!-- ignore -->
pub inline fn ignore_when_sema() void {
    // utility function for semantic analysis
    //
    // causes code in the function beyond the point to be be
    // ignored during semantic analysis passes.
    if (is_sema) unreachable;
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
/// retrievable from [`term`](#term) using the [`term.term_type`](#term-term_type) method.
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
pub const term = if (is_sema) struct {
    v: e.ErlNifTerm,
    pub fn term_type(_: *const @This(), _: env) TermType {
        return .atom;
    }
} else packed struct {
    v: e.ErlNifTerm,

    /// equivalent of e.enif_term_type
    pub fn term_type(this: *const @This(), environment: env) TermType {
        return @intToEnum(TermType, e.enif_term_type(environment, this.v));
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

/// <!-- topic: Term Management; args: dest_type, _, source_term, options -->
/// converts BEAM [`term`](#term) dynamic types into static zig types
///
/// The arguments are as follows:
/// 1. destination type
/// 2. [environment](#env)
/// 3. term to convert
/// 4. struct (usually passed as anonymous) of keyword options for additional features.
///   See [supported options](#get-supported-options)
///
/// See also [`make`](#make) for the reverse operation.
///
/// The following type classes (as passed as 1st argument) are supported by `get`:
///
/// ### integer
/// - unsigned and signed integers supported
/// - all integer sizes from 0..64 bits supported (including non-power-of-2
///   sizes)
/// - for sizes bigger than 64, supported, but the passed term must be a
///   native-endian binary.
///
/// #### Example
///
/// ```elixir
/// do_get(47)
/// ```
///
/// ```zig
/// pub fn do_get(env: beam.env, term: beam.term) void {
///     const x: i32 = beam.get(i32, env, term, .{});  // -> x = 47
///     ...
/// }
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
/// do_get(:foo)
/// ```
///
/// ```zig
/// const EnumType = enum {foo, bar};
/// pub fn do_get(env: beam.env, term: beam.term) void {
///     const x: EnumType = beam.get(EnumType, env, term, .{});  // -> x = .foo
///     ...
/// }
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
/// do_get(47.0)
/// ```
///
/// ```zig
/// pub fn do_get(env: beam.env, term: beam.term) void {
///     const x: f32 = beam.get(f32, env, term, .{});  // -> x = 47.0
///     ...
/// }
/// ```
///
/// ### struct
/// - may be passed `t:map/0` with `t:atom/0` keys and values of the appropriate type
/// - may be passed a `t:keyword/0` list with `t:atom/0` keys and values of the
///   appropriate type.
/// - inner values are recursively converted to the appropriate type.
/// - NOTE: the struct types must be exported as `pub` in the module's
///   top-level namespace.
/// - if the struct is `packed` or `extern`, supports binary data.
///
/// #### Example
///
/// ```elixir
/// do_get(%{foo: 47, bar: %{baz: :quux}})
/// ```
///
/// ```zig
/// pub const EnumType = enum {quux, mlem};
///
/// pub const InnerType = struct {
///   baz: EnumType,
/// };
///
/// pub const StructType = struct {
///   foo: i32,
///   bar: InnerType
/// };
///
/// pub fn do_get(env: beam.env, term: beam.term) void {
///     const x = beam.get(StructType, env, term, .{});  // -> x = .{foo: 47, bar: .{baz: .quux}}
///     ...
/// }
/// ```
///
/// ### bool
/// - supports `true` and `false` `t:boolean/0` terms only.
///
/// #### Example
///
/// ```elixir
/// do_get(true)
/// ```
///
/// ```zig
/// pub fn do_get(env: beam.env, term: beam.term) void {
///     const x: bool = beam.get(bool, env, term, .{});  // -> x = true
///     ...
/// }
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
///
/// #### Example
///
/// ```elixir
/// do_get([47, 48, 49])
/// do_get(<<47 :: signed-int-size(32), 48 :: signed-int-size(32), 49 :: signed-int-size(32)>>)
/// ```
///
/// ```zig
/// pub fn do_get(env: beam.env, term: beam.term) void {
///     const x = beam.get([3]i32, env, term, .{});  // -> x = .{47, 48, 49}
///     ...
/// }
/// ```
///
/// ### single-item pointer
/// - allocates memory based on allocator provided in the options, otherwise
///   defaults to [`beam.allocator`](#allocator)
/// - supports any type as above.
/// - returns an error if the allocation fails.
///
/// #### Example
///
/// ```elixir
/// do_get(%{foo: 47})
/// ```
///
/// ```zig
/// const MyStruct = struct { foo: i32 };
///
/// pub fn do_get(env: beam.env, term: beam.term) void {
///     const x = beam.get(*MyStruct, env, term, .{});  // -> x = a pointer to .{.foo = 47}
///     ...
/// }
/// ```
///
/// ### slice
/// - allocates memory based on allocator provided in the options, otherwise
///   defaults to [`beam.allocator`](#allocator)
/// - note that slice carries a runtime length
/// - supports list of any type
/// - supports binary of any type that can be represented as a fixed size binary.
///
/// #### Example
///
/// ```elixir
/// do_get([47, 48, 49])
/// do_get(<<47 :: signed-int-size(32), 48 :: signed-int-size(32), 49 :: signed-int-size(32)>>)
/// ```
///
/// ```zig
/// pub fn do_get(env: beam.env, term: beam.term) void {
///     const x = beam.get([]i32, env, term, .{});  // -> x = a pointer to .{47, 48, 49}
///     ...
/// }
/// ```
///
/// ### many-item-pointer
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
///     > datatype should be handled with extreme care.
///
/// #### Example
///
/// ```elixir
/// do_get([47, 48, 49])
/// do_get(<<47 :: signed-int-size(32), 48 :: signed-int-size(32), 49 :: signed-int-size(32)>>)
/// ```
///
/// ```zig
/// pub fn do_get(env: beam.env, term: beam.term) void {
///     const x = beam.get([*]i32, env, term, .{});  // -> x = a pointer to .{47, 48, 49}
///     ...
/// }
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
///
/// #### Example
///
/// ```elixir
/// do_get([47, 48, 49])
/// do_get(<<47 :: signed-int-size(32), 48 :: signed-int-size(32), 49 :: signed-int-size(32)>>)
/// ```
///
/// ```zig
/// pub fn do_get(env: beam.env, term: beam.term) void {
///     const x = beam.get([*]i32, env, term, .{});  // -> x = a pointer to .{47, 48, 49}
///     ...
/// }
/// ```
///
/// ### optional
///
/// - accepts `t:atom/0` `nil` as well as whatever the child type is.
/// - note that zig has `null` so `nil` will get converted to `null`.
///
/// #### Example
///
/// ```elixir
/// do_get(nil)
/// ```
///
/// ```zig
/// pub fn do_get(env: beam.env, term: beam.term) void {
///     const x = beam.get(?i32, env, term, .{});  // -> x = null
///     ...
/// }
/// ```
///
/// ## Supported options
///
/// - `allocator`: the allocator to use for allocations.  If not provided, defaults
///   to `beam.allocator`.
/// - `error_info`: pointer to a [`term`](#term) that can be populated with error
///   information that gets propagated on failure to convert.  If omitted, the code
///   to produce these errors will get optimized out.
pub const get = get_.get;

/// <!-- topic: Term Management; args: _, value, options -->
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
/// ### `void`
/// - returns atom `:ok`
/// - supporting this type makes metaprogramming easier.
///
/// ### [`pid`](#pid)
/// - convrted into a [`term`](#term) representing `t:pid/0`
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
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   return beam.make(env, 47, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> 47
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
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   return beam.make(env, 47.0, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> 47.0
/// ```
///
/// ### bool
/// - supports `bool` types.
/// - returns a BEAM `t:boolean/0` term
///
/// #### Example
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   return beam.make(env, true, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> true
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
/// with an enum:
///
/// ```zig
/// const EnumType = enum {foo, bar};
///
/// pub fn do_make(env: beam.env) beam.term {
///   const e = EnumType.foo;
///   return beam.make(env, e, .{});
/// }
/// ```
///
/// with an error enum:
///
/// ```zig
/// const ErrorType = error {foo, bar};
///
/// pub fn do_make(env: beam.env) beam.term {
///   return beam.make(env, error.foo, .{});
/// }
/// ```
///
/// with an enum literal:
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   return beam.make(env, .foo, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> :foo
/// ```
///
/// > ### Enum literals {: .info }
/// >
/// > Enum literals are especially useful for returning atoms,
/// > such as `:ok` or `:error`.  Note that `error` is a reserved
/// > word in zig, so you will need to use `.@"error"` to generate
/// > the corresponding atom.
///
/// ### optionals or null
/// - supports any child type supported by [`make`](#make)
/// - returns the `t:atom/0` type `nil` or the child type
///
/// #### Example
///
/// with null literal:
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   return beam.make(env, null, .{});
/// }
/// ```
///
/// with an optional type:
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   const value: ?i32 = null;
///   return beam.make(env, value, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> null
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
///   data, by setting `output_type` option to `.binary`
/// - if the array's element is u8 and you would prefer outputting as a list,
///   setting `output_type` option to `.list` will do this.
///
/// #### Examples
///
/// array, u8, output as `t:binary/0`:
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   return beam.make(env, "foo", .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> "foo"
/// ```
///
/// array, u8, output as `t:list/0`:
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   return beam.make(env, "foo", .{.output_type = .list});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> ~C'foo'
/// ```
///
/// array, u16, output as `t:list/0`:
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   const list = [_]u16{47, 48, 49}
///   return beam.make(env, list, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> [47, 48, 49]
/// ```
///
/// array, u16, output as `t:binary/0`:
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   const list = [_]u16{47, 48, 49}
///   return beam.make(env, list, .{.output_type = .binary});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> <<47, 00, 48, 00, 49, 00>>
/// ```
///
/// ### structs
///
/// - supports structs with fields of any term that can be encoded using [`make`](#make)
/// - outputs as a `t:map/0` with atom keys and the encoded terms as values
/// - for packed or extern structs, supports binary data by setting `output_type`
///   option to `.binary`
/// - encoding options are passed recursively, if something more complex is needed,
///   encoding should be performed manually.
/// - supports anonymous structs
///
/// #### Examples
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   return beam.make(env, .{.foo = 123, .bar = "bar", .baz = .baz}, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> %{foo: 123, bar: "bar", baz: :baz}
/// ```
///
/// ### tuples
///
/// - supports tuples with any term that can be encoded using [`make`](#make)
/// - outputs as a `t:tuple/0`.
/// - encoding options are passed recursively, if something more complex is needed,
///   encoding should be performed manually.
/// - note that error atom should be encoded as `.@"error"`
///
/// #### Examples
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   return beam.make(env, .{.ok, "foo", 47}, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> {:ok, "foo", 47}
/// ```
///
/// ### single-item-pointer
///
/// - these pointers are only supported for arrays and structs
/// - these are only supported because they are assumed to be pointers to
///   mutable data
/// - content will be dereferenced and encoded as if it were the child type
/// - `output_type` rules (see [arrays](#make-arrays)) apply.
///
/// #### Examples
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   const array = [_]i32{47, 48, 49}
///   return beam.make(env, &array, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> [47, 48, 49]
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
///   data, by setting `output_type` option to `.binary`
/// - `output_type` rules (see [arrays](#make-arrays)) apply.
///
/// #### Examples
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   const slice = [_]i32{47, 48, 49}[0..]; // note this is now a slice
///   return beam.make(env, &slice, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> [47, 48, 49]
/// ```
///
/// ### many-item-pointer
///
/// - only supported if the pointer is sentinel-terminated.
/// - outputs as a list of the encoded terms
/// - pointers of `u8` default to outputting binary, this is the only exception
///   to the above rule.
/// - if the pointers's element is integers, floats, packed or extern structs,
///   or arrays that support binaries, then the slice can be output as binary
///   data, by setting `output_type` option to `.binary`
/// - `output_type` rules (see [arrays](#make-arrays)) apply.
///
/// #### Examples
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   const slice = [_]i32{47, 48, 49, 0}[0..];
///   const ptr = @ptrCast([*:0], &slice.ptr);
///   return beam.make(env, &slice, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> [47, 48, 49]
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
/// - `output_type` rules (see [arrays](#make-arrays)) apply.
///
/// #### Examples
///
/// ```zig
/// pub fn do_make(env: beam.env) beam.term {
///   const slice = [_]i32{47, 48, 49, 0}[0..];
///   const ptr = @ptrCast([*:0], &slice.ptr);
///   return beam.make(env, &slice, .{});
/// }
/// ```
///
/// ```elixir
/// do_make() # -> [47, 48, 49]
/// ```
pub const make = make_.make;

// special makers

/// <!-- topic: Term Management -->
/// turns a [`e.ErlNifPid`](https://www.erlang.org/doc/man/erl_nif.html#ErlNifPid)
/// into a `t:pid/0` term.
///
/// This is a thin wrapper over [`e.enif_make_pid`](https://www.erlang.org/doc/man/erl_nif.html#enif_make_pid).
pub const make_pid = make_.make_pid;

/// <!-- topic: Term Management; args: _, string -->
/// turns a `[]const u8` into a the corresponding `t:atom/0` term.
///
/// returns a raised `ArgumentError` if the length of the string exceeds the
/// vm atom size limit (255 bytes)
///
/// This is a thin wrapper over [`e.enif_make_atom_len`](https://www.erlang.org/doc/man/erl_nif.html#enif_make_atom_len).
pub const make_into_atom = make_.make_into_atom;

/// <!-- topic: Term Management -->
/// returns the empty list term `[]`.
///
/// This is a thin wrapper over [`e.enif_make_empty_list`](https://www.erlang.org/doc/man/erl_nif.html#enif_make_empty_list).
pub const make_empty_list = make_.make_empty_list;

/// <!-- topic: Term Management; args: _, head, tail -->
/// performs a list cons operation for `head` and `tail` variables
///
/// This is a thin wrapper over [`e.enif_make_list_cell`](https://www.erlang.org/doc/man/erl_nif.html#enif_make_list_cell).
pub const make_list_cell = make_.make_list_cell;

/// <!-- topic: Term Management; args: env -->
/// shortcut for `make(env, .@"error", .{})`
pub const make_error_atom = make_.make_error_atom;

/// <!-- topic: Term Management; args: env, value, options -->
/// shortcut for `make(env, .{.@"error", value}, options)`
pub const make_error_pair = make_.make_error_pair;

/// <!-- topic: Term Management -->
/// causes the VM to generate a new reference term
/// equivalent to `Kernel.make_ref/0`
///
/// This is a thin wrapper over [`e.enif_make_ref`](https://www.erlang.org/doc/man/erl_nif.html#enif_make_ref).
pub const make_ref = make_.make_ref;

/// <!-- topic: Term Management -->
///
/// converts a zig `std.builtin.StackTrace` into a special term
/// that is designed to be translated and concatenated onto a BEAM
/// stacktrace.
///
/// ### Example term:
///
/// ```elixir
/// [
///   %{
///     line_info: %{file_name: "/path/to/project/lib/my_app/.Elixir.MyApp.MyModule.zig", line: 15},
///     symbol_name: "my_fun",
///     compile_unit_name: "Elixir.MyApp.MyModule"
///   }
///   ...
/// ]
/// ```
pub const make_stacktrace = stacktrace.to_term;

/// <!-- topic: Term Management -->
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
pub const self = processes.self;

/// <!-- topic: Term Management; args: _, _, data -->
/// sends `data` (as a term) to a target process' mailbox.
///
/// equivalent to `Kernel.send/2`
///
/// This function is a context-aware wrapper over
/// [`e.enif_send`](https://www.erlang.org/doc/man/erl_nif.html#enif_send).
/// that also serializes the message term using [`make`](#make)
///
/// > ### send from raw nifs {: .warning }
/// >
/// > This function has undefined behaviour when called from `raw` nifs.
/// > use `e.enif_send` directly instead.
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
///   optional, defaults to the threadlocal [`allocator`](#allocator) value
pub const cleanup = cleanup_.cleanup;

// comparisons

/// result type for [`compare`](#compare)
///
/// these atoms are used to conform to Elixir's Compare interface
/// see: https://hexdocs.pm/elixir/1.13/Enum.html#sort/2-sorting-structs
pub const Compared = enum { lt, eq, gt };

/// <!-- topic: Term Management -->
/// compares two terms.
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

/// <!-- topic: Term Management; args: _, string -->
///
/// converts a `[]u8` to a `t:term/0`.  The string must be encoded using erlang term format.
///
/// This is a thin wrapper over [`e.enif_binary_to_term`](https://www.erlang.org/doc/man/erl_nif.html#enif_binary_to_term).
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

pub const make_general_purpose_allocator_instance = allocator_.make_general_purpose_allocator_instance;

/// provides a BEAM allocator that can perform allocations with greater
/// alignment than the machine word.
///
/// > #### Memory performance {: .warning }
/// >
/// > This comes at the cost of some memory to store metadata
///
/// currently does not release memory that is resized.  For this behaviour
/// use `beam.general_purpose_allocator`.
///
/// not threadsafe.  for a threadsafe allocator, use `beam.general_purpose_allocator`
pub const large_allocator = allocator_.large_allocator;

/// implements `std.mem.Allocator` using the `std.mem.GeneralPurposeAllocator`
/// factory, backed by `beam.large_allocator`.
pub const general_purpose_allocator = allocator_.general_purpose_allocator;

/// wraps [`e.enif_alloc`](https://www.erlang.org/doc/man/erl_nif.html#enif_alloc)
/// and [`e.enif_free`](https://www.erlang.org/doc/man/erl_nif.html#enif_free)
/// into the zig standard library allocator interface.
pub const raw_allocator = allocator_.raw_allocator;

/// stores the allocator strategy for the currently running nif.
///
/// this variable is threadlocal, so that each called NIF can set it as a
/// global variable and not pass it around.
///
/// > #### allocator starts undefined {: .warning }
/// >
/// > This threadlocal is set to `undefined` because of architectural
/// > differences:  we cannot trust loaded dynamic libraries to properly set
/// > this on thread creation.  Each function is responsible for setting
/// > allocator correctly whenever execution control is returned to it.
/// >
/// > `raw` function calls do *not* set the allocator and
/// > must either set it themselves or always use a specific allocator
/// > strategy in its function calls.
pub threadlocal var allocator: std.mem.Allocator = undefined;

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
pub const alloc_env = e.enif_alloc_env;

/// <!-- topic: Env -->
/// Synonym for [`e.enif_free_env`](https://www.erlang.org/doc/man/erl_nif.html#enif_free_env)
pub const free_env = e.enif_free_env;

/// <!-- topic: Env -->
/// copies a term from one env to to another
pub fn copy(env_: env, term_: term) term {
    return .{ .v = e.enif_make_copy(env_, term_.v) };
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

/// a tag identifying the context in which the nif is running
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
///
/// See [`context`](#context) for the threadlocal variable that stores this.
///
/// > #### raw beam functions {: .warning }
/// >
/// > nifs called in `raw` mode are not assigned an execution context.
pub const ExecutionContext = enum { synchronous, threaded, dirty, yielding, callback };

/// threadlocal variable that stores the execution context for the nif
///
/// See [`ExecutionContext`](#executioncontext) for the list of valid enums.
///
/// > #### context starts undefined {: .warning }
/// >
/// > This threadlocal is set to `undefined` because of architectural differences:
/// > we cannot trust loaded dynamic libraries to properly set this on thread
/// > creation.
/// >
/// > `raw` function calls do not set `context`
pub threadlocal var context: ExecutionContext = undefined;

const yield_ = @import("yield.zig");

/// <!-- topic: Concurrency -->
/// periodic `check-in` function for long-running nifs.
///
/// For threaded nifs:
///
/// - checks the status of the thread.
/// - If the thread's parent process has been killed, returns
///   `error.processterminated`
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
/// For synchronous or dirty nifs:
///
/// - does nothing.
/// - there may be a slight performance regression as the function
///   identifies the concurrency mode of the nif.
pub const yield = yield_.yield;

//pub const YieldingFrame = yield_.Frame;
//pub const YieldingCallbacks = yield_.Callbacks;

// wrappedresult: for yielding and threaded nifs we have to do something a bit
// different to wrap a zig error across the beam boundary.  This common utility
// type is used for that.

const WrappedResultTag = enum { ok, error_return_trace };

/// <!-- ignore -->
pub fn WrappedResult(comptime FunctionType: type) type {
    const NaiveReturnType = @typeInfo(FunctionType).Fn.return_type.?;
    return switch (@typeInfo(NaiveReturnType)) {
        .ErrorUnion => |eu| union(WrappedResultTag) {
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
pub fn raise_exception(env_: env, reason: anytype) term {
    return term{ .v = e.enif_raise_exception(env_, make(env_, reason, .{}).v) };
}

/// <!-- topic: Exceptions -->
///
/// The equivalent of `Kernel.raise/1` from elixir.
///
/// - `module` should be the name of the module that represents the exception
/// - `data` should be a struct (possibly anonymous) that represents the Elixir
///   exception payload.
///
/// > #### Exception structs are not checked {: .warning }
/// >
/// > The validity of the exception struct is not checked when using this function.
pub fn raise_elixir_exception(env_: env, comptime module: []const u8, data: anytype) term {
    if (@typeInfo(@TypeOf(data)) != .Struct) {
        @compileError("elixir exceptions must be structs");
    }

    const name = comptime name: {
        break :name "Elixir." ++ module;
    };
    var exception: e.ErlNifTerm = undefined;
    const initial = make(env_, data, .{});

    _ = e.enif_make_map_put(env_, initial.v, make_into_atom(env_, "__struct__").v, make_into_atom(env_, name).v, &exception);
    _ = e.enif_make_map_put(env_, exception, make_into_atom(env_, "__exception__").v, make(env_, true, .{}).v, &exception);

    return raise_exception(env_, term{ .v = exception });
}

/// <!-- topic: Exceptions -->
/// Raises a special error datatype that contains an term-encoded stacktrace
/// datastructure.  See also [`make_stacktrace`](#make_stacktrace)
///
/// This datastructure is designed to be concatenated onto the existing
/// stacktrace.  In order to concatenate this stacktrace onto your BEAM
/// exception, the function that wraps the nif must be able to catch the
/// error and append the zig error return trace to the existing stacktrace.
pub fn raise_with_error_return(env_: env, err: anytype, maybe_return_trace: ?*std.builtin.StackTrace) term {
    if (maybe_return_trace) |return_trace| {
        return raise_exception(env_, .{ .@"error", err, return_trace });
    } else {
        return raise_exception(env_, .{ .@"error", err });
    }
}

// unignore this on 0.11, if this is validated

/// <!-- ignore -->
pub const Mutex = @import("mutex.zig").Mutex;
