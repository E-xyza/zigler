# Translate-C

A Zig package for translating C code into Zig code, intended to replace
`@cImport` and `zig translate-c`.

This branch tracks Zig 0.17.x. Other branches track other versions of Zig.

## Usage

Add `translate-c` to your `build.zig.zon` with this command:

```
$ zig fetch --save git+https://codeberg.org/ziglang/translate-c#zig-0.17.x
info: resolved to commit 0da7a16c3235b935b82421646076e0657cda21f6
```

Then, within your `build.zig`, write something like this:

```zig
// An abstraction to make using translate-c as simple as possible.
const Translator = @import("translate_c").Translator;

const translate_c = b.dependency("translate_c", .{});

const t: Translator = .init(translate_c, .{
    .c_source_file = b.path("to_translate.h"),
    .target = target,
    // This is the optimization mode of the C code being translated and
    // the resulting Zig code.
    .optimize = optimize,
    // more options go here (see below)
});
// If you want, you can now call methods on `Translator` to add include paths (etc).

// Depend on the translated C code as a Zig module.
some_module.addImport("translated", t.mod);
// ...or, if you want to, just use the output file directly.
const translated_to_zig: LazyPath = t.output_file;
```

For a more complete usage, take a look at the `examples/` directory.

## Options

```zig
/// Should static functions be translated as `pub`.
pub_static: bool,
/// Should function bodies be translated.
func_bodies: bool,
/// Should macro names of literals be preserved.
keep_macro_literals: bool,
/// Should struct fields be default initialized.
default_init: bool,
/// Control when to treat a trailing array as a flexible array member.
/// Mirrors the -fstrict-flex-arrays=<n> compiler flag.
strict_flex_arrays: StrictFlexArraysLevel,

pub const StrictFlexArraysLevel = enum {
    /// Any trailing array member is a flexible array.
    @"0",
    /// Trailing arrays of size 0, 1, or undefined are flexible.
    @"1",
    /// Trailing arrays of size 0 or undefined are flexible (default).
    @"2",
    /// Only trailing arrays of undefined size are flexible.
    @"3",
};
```
