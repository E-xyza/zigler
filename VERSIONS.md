# Zig versions

## 0.1.0

- zig compilation and automatic nif generation
- safety checks on ingress and egress data
- documentation of zig code through ExDoc
- unit test integration

## 0.1.1

Thanks to Dave Cottlehuber @dch for testing.

- freebsd tested and unsupported Logger error removed.
- zig documentation is now correctly linked.

## 0.1.2

- updated documentation for `mix zig_doc`
- fixed compilation so that importing zig stdlib doesn't error
- added better compilation failures for bad types in args and retvals
- added `beam` struct support for ok and error tuples

## 0.1.3

- fixed error reporting for the `/// nif:` directive
- added ok/error tuple with string
- added c header path support
- restored documentation for exceptions

## 0.2.0

- general rewrite of nif parsing routines
- initial support for resources
- experimental addition of `:dirty`, `:long` and `:safe` function directives

## 0.3.0-pre

- supported zig version: 0.6.0
- disabled `:long` mode

## 0.3.0-pre3

- support for (and autodetect) nerves
- use erlang nif headers taken from the `include` directory of your erts.
- correct and verified support for releases
- adds a mechanism for patching errors in zig/llvm releases
- adds ~z for substituted values
- zigler fetches zig on compile when it's in prod.

## 0.3.0

- verified that zig tests run correctly
- updated documentation.
  - preliminary "under the hood" category.

## 0.3.1

- support for `amd64` in freebsd, (thanks @dch)

## 0.3.2

- better support for nerves (thanks @fhunleth)
- fixed documentation linking for bootstrap

## 0.7.1
- upgraded to zig 0.7.1.  From now on zigler version numbers will track Zig
  releases.
- completed implementation of `threaded` nifs.
- makes the interface to `beam.send` simpler and less confusing.
- adds extra atom/tuple directives
- **important:** changes the base module namespace from `Zigler` to `Zig`
- readme and guides are now checked.
- libraries are now brought into the staging directory.
- unblocked windows compilation path.  There are still some bugs. Thanks
  @seomwan
- regressions:
  - Nerves support will have to be dropped until arm32 is tier 1.  Use 0.3.2.
  - zigtests do not seem to work when libraries are loaded

## 0.7.2
- **note:** this compiles with zig 0.7.1
- **important (breaking):** semantics for `beam.yield` have changed.
  there is no longer any need to update the environment of the yielding
  function, and `beam.yield()` now returns `!void`
- `beam.yield` is safely usable in non-yielding nifs.
- Zigler now no longer automatically links libc.  This may cause
  compilation problems on non-linux platforms.  Please post any bug
  reports to github tracker.
- thanks to (@dch and @wojtekmach for updating some dependency issues)

## 0.7.3
- **note:** this compiles with zig 0.7.1
- better platform matching for nerves cross-compliation (@fhunleth)

## 0.8.1
- `beam.assert` deprecated.  Use `std.testing` functions.
- support for error-returning functions and error return traces merged with
  stacktraces.
- support for cancellation of threaded nifs with `beam.yield`
- better support for 'bring your own zig'; moved to library config function
- tentative support for MacOS and FreeBSD targets
- `void` returning functions now output `:ok` instead of `nil`

## 0.9.1
- updated to zig 0.9.1, after allocgate
- support for compling C and C++ files using `zig cc`
- support for `link_libcpp` in addition to `link_libc`
- special thanks to @jeremyowensboggs, who had a nicer upgrade implementation
  than mine.

## 0.10.1
- complete overhaul of entire zig system
- Breaking changes:
  - `beam.get`, `beam.make`
  - nif selection is no longer done using inline docstrings
    - select nifs using `use Zig` options.
  - zig test has been deprecated, and may return as a plugin
    library in the future
  - zig doc has been deprecated for general use, though can
    be used experimentally through `Zig.Doc`
  - yielding nifs are not supported, but will return in 0.11
    due to zig async not working in 0.10.x

## 0.12.1
- breakout `zig.get` into its own package to prevent dependency problems.
- breakout `zig.doc` into its own package to prevent dependency problems.
- versioning of .so files should track the version.  This feature should be considered experimental.
- use of threadlocal `beam.context` to keep contextual information for your NIF
  - major changes in `beam.get` and `beam.make`
  - `.as` option for `beam.make` with recursive descent
- full range of `on_load`, `on_update`, and `on_reload` functions
- optional allocator functions usable with nifs
- better recursive typing
- smarter `beam.send`
- attributes imported as compile-time values

## 0.13.0
- updated to zig 0.13.0
- Breaking changes:
  - threadlocal `context` in function calls, optional passing `beam.env` into your functions
  - makes `beam.get/make` functions `context`-aware, with all context-dependent things overridable.

## 0.14.0
- Breaking changes:
  - deprecation of General Purpose Allocator in favor of DebugAllocator
  - beam.term_type now uses an options tuple with `env` field.
  - struct module names are now assigned using the `struct` option instead of using `as`.
- Windows support:
  - Experimental windows support added.
  - Error return traces not supported.
  - on_upgrade hooks not supported pending module .so versioning changes.
- Features:
  - adds ZIG_EXECUTABLE_PATH environment variable
  - path resolution from CWD when a path is specified as `./`
  - adds the ability to override the default fallback function (elixir only)
- Outstanding issues:
  - c_ulonglong doesn't work on windows; c_long doesn't work on other platforms.
- Etc:
  - uses system `:json` module, if available.
  - Experimental FreeBSD support added.

## 0.14.1

- no changes.

## 0.15.2

- Breaking changes:
  - rename "packages" option to "extra_modules"
  - unification of path scheme ("./" is project-rooted)
  - renamed `release_mode` to `optimize`.  Also note the new optimize policy defaults
    to `ReleaseSafe` in non-dev, non-test modes and defaults to `Debug` in test and dev.
- Policy changes:
  - error return traces by default in ReleaseSafe builds
- Temporary Regressions:
  - Error return traces are disabled in MacOS (due to https://github.com/ziglang/zig/issues/25433)
  - on_upgrade tests are failing in elixir 1.19.x; may or may not reflect a real problem.
- Outstanding issues:
  - c_ulonglong doesn't work on windows; c_long doesn't work on other platforms.
- Features:
  - add "dependency" option for zig dependencies
  - `precompiled` option that will let you use precompiled library assets.  NOTE: this feature is highly
    experimental and functions might need to be compiled for different erts versions.  This will likely 
    be changed in the future.
  - adds `beam.get_list_cell`

## 0.17.0

- Breaking changes:
  - Updated to Zig 0.17.0.  You will need to install the new toolchain:
    `mix zig.get`.
  - Requires Elixir 1.18 or later (was 1.15).
  - **`@hasDecl` only sees public declarations.**  Resource `Callbacks` structs must
    declare `pub fn dtor`, `pub fn stop`, `pub fn down` and `pub fn dyncall`; a
    callback that is not `pub` is now silently not detected.
  - Zig 0.17 removed the `**` array repetition operator (use `@splat`), the
    `errdefer |err|` capture, `void{}`, and the `i0` type.  `i0` is therefore no
    longer a supported nif parameter or return type.
  - `std.builtin` is deprecated in favour of `std.lang` (it remains as an alias).
    Nif code using `std.builtin.Type`, `std.builtin.StackTrace` and friends should
    move to `std.lang`.
  - `std.meta.Int` was removed in favour of the new `@Int` builtin.
  - Optimize mode tags lost their `Release` prefix: `@import("builtin").mode` now
    reports `.debug`/`.safe`/`.fast`/`.small`.  Zigler's own `optimize:` option is
    unchanged, as it already used those names.
  - C header translation now goes through the ZSF `translate-c` package, since Zig
    0.17 deprecates the built-in `std.Build.Step.TranslateC`.  Zigler fetches
    `translate-c` and `arocc` as source-only git dependencies -- pinned in
    `mix.lock` -- and stages them beside the generated `build.zig`, so compiling a
    nif still requires no network access.  If you supply your own build files with
    `build_files_dir:`, use the `Translator` API instead of `b.addTranslateC`;
    zigler declares the `translate_c` package in your `build.zig.zon` for you.
- Features:
  - `mix zig.warm` precompiles `translate-c` into the global zig cache.  Nothing
    needs to call it -- the first nif compiled does the work, and zig's cache lock
    means concurrent builds wait rather than duplicate it -- but it is useful in CI,
    where that one-off minute would otherwise land inside a test.
- Fixes:
  - Source locations in error return traces are resolved correctly again on macos
    and freebsd, where Zig 0.17 reports paths relative to the build root.
  - Cross-compiling to Windows works: the windows `erl_nif` headers are selected
    from the *target*, not the build host, and `translate-c` is built for the host
    rather than the target.
- Dependencies:
  - `zig_parser` 0.8.0, `zig_doc` 0.8.0 and `zig_get` 0.17.0, all of which track
    Zig versions.
- Internal:
  - `@typeInfo` reports struct, union and enum members as parallel arrays rather
    than an array of per-field records.  `priv/beam/reflect.zig` is a new module
    that rebuilds the previous per-field view for the marshalling code.
  - `priv/erl_nif_win/erl_nif_win.h` now documents why those headers are vendored,
    and what to re-check when `translate-c` improves.

## 0.16.0

- Breaking changes:
  - Updated to Zig 0.16.0
  - `@cImport` replaced with `addTranslateC` - use `headers:` option instead
- Features:
  - `headers:` option for C integration - automatically translate C headers to Zig modules
  - `build_flags:` option for passing custom `-D` flags to `zig build`
  - `build_files_dir:` option for providing custom `build.zig` files
  - `beam.io` module providing `std.Io` interface for Zig 0.16.0 with BEAM-specific overrides:
    - Time functions use `enif_monotonic_time`/`enif_time_offset` for BEAM-consistent time
    - No-op stderr locking (can't coordinate with BEAM's stderr)
    - Disabled environ (use `os:getenv/1` or `System.get_env/1` and pass as NIF parameter)
  - Error return traces re-enabled for macOS and Windows
  - Runtime library rpaths support
  - Erlang typespec support: `-spec` annotations are now generated for Erlang modules
  - Tuple support: `get_tuple` semantics for receiving Elixir tuples, encoding options for tuple elements
  - `leak_check` support for threaded functions
  - Support for `[*c]` to sentinel-terminated types
  - Experimental OpenBSD support for precompiled sema extraction
- Fixes:
  - Cross-compilation sema now uses host target correctly
  - Windows: PDB packaging for debug symbols
  - Windows: Error stacktrace handling improvements
  - Windows: Threaded NIF hang fix in Io.Dir.openFile
  - Error if `struct:` option used on non-struct return type
  - Fix optional manypointer cleanup and size handling
  - Fix sub-binary alignment for slices of wider types
  - Fix threaded NIF initialization error handling
- Documentation:
  - Document threaded NIF cleanup behavior and 750µs timeout

## Possible Future Changes
- `struct` which lets you declare a struct inside your zig code as the module's struct
