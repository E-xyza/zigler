defmodule Zig do
  @moduledoc """

  Inline NIF support for [Zig](https://ziglang.org)

  For erlang support see documentation for the [:zigler](:zigler.html) module.

  ### Motivation

  > Zig is a general-purpose programming language designed for robustness,
  > optimality, and maintainability.

  The programming philosophy of Zig matches up nicely with the programming
  philosophy of the BEAM VM and in particular its emphasis on simplicity and
  structure should very appealing to the practitioners of Elixir.

  The following features make Zig extremely amenable to inline language
  support in a BEAM language:

  - Simplicity.  Zig's syntax is definable in a simple YACC document and
    Zig takes a stance against making its featureset more complex (though
    it may evolve somewhat en route to 1.0)
  - Composability.  Zig is unopinionated about how to go about memory
    allocations.  Its allocator interface is very easily able to be backed
    by the BEAM's, which means that you have access to generic memory
    allocation *strategies* through its composable allocator scheme.
  - C integration.  It's very easy to design C-interop between Zig and C.
    Zigler has been designed to make it easier to use Zigler to build
    C libraries than to use C directly see [Easy C](#module-easy-c).

  ### Guides

  Please consult the following guides for detailed topics:

  - [Using Nifs](01-nifs.html)
  - [Collection datatypes](02-collections.html)
  - [Allocator strategies](03-allocators.html)
  - [Nif options](04-nif_options.html)
  - [Resources](05-resources.html)
  - [C integration](06-c_integration.html)
  - [Concurrency strategies](07-concurrency.html)
  - [Global module options](08-module_options.html)
  - [Raw calling](09-raw_nifs.html)
  - [Module callbacks](10-callbacks.html)
  - [Precompiled packages](11-precompiled.html)]

  > ### Zig version support {: .warning }
  >
  > although the large-scale archictecture of zigler is settled,
  > zigler features may break backwards compatibility until zig reaches
  > 1.0

  ### Nerves Support

  Nerves is supported out of the box, and Zigler will be able to seamlessly
  detect the cross-compilation information (os, architecture, runtime) and
  build correctly for that target.

  > ### Nerves warnings {: .warning }
  >
  > Note that when compiling for nerves, you may encounter warnings about modules
  > being unable to be loaded.  This is because the system is cross-compiling the
  > module for a different target architecture and this is normal behavior.

  ### Basic NIFs

  In the BEAM, you can define a NIF by consulting the following [document](
  https://erlang.org/doc/man/erl_nif.html) and implementing the appropriate
  shared object/DLL callbacks.  However, Zigler will take care of all of
  this for you.

  Simply `use Zig` in your module, providing the otp_app name as an option.

  Then, use the `sigil_Z/2` macro and write inline zig code.  To present a
  function as a nif in your module, simply export it from your code namespace
  by making it a `pub` function in your zig code.

  #### Example

  ```elixir
  defmodule BasicModule do
    use Zig, otp_app: :zigler

    ~Z\"""
    pub fn add_one(number: i64) i64 {
        return number + 1;
    }
    \"""
  end

  test "basic module with nif" do
    assert 48 = BasicModule.add_one(47)
  end
  ```

  > #### otp_app setting {: .info }
  >
  > You should replace `:zigler` in the following example with the name of
  > your own app.  If no such app exists (e.g. you are using livebook or
  > are in the terminal or escript), you can use `:zigler` as a fallback.

  Zigler will *automatically* fill out the appropriate NIF C template, compile
  the shared object, and bind it into the module pre-compilation. In the above
  example, there will be a `BasiceModule.add_one/1` function call created.

  Zigler will also make sure that your statically-typed Zig data are guarded
  when you marshal it from the dynamically-typed BEAM world.  However, you may
  only pass in and return certain types.  As an escape hatch, you may use
  the [`beam.term`](beam.html#term) type which is a wrapped
  [`ERL_NIF_TERM`](https://www.erlang.org/doc/man/erl_nif.html#ERL_NIF_TERM) type.
  See [`erl_nif`](https://www.erlang.org/doc/man/erl_nif.html).

  ```elixir
  test "argument error when types are mismatched" do
    assert_raise ArgumentError, fn -> BasicModule.add_one("not a number") end
  end
  ```

  ### I don't want to use inline Zig

  ```zig
  \\\\ .noinline.zig
  pub fn add_one(number: i64) i64 {
      return number + 1;
  }
  ```

  ```elixir
  defmodule NoInline do
    use Zig, otp_app: :zigler, zig_code_path: ".noinline.zig"
  end

  test "non-inline zig" do
    assert 48 = NoInline.add_one(47)
  end
  ```

  ### Advanced usage: Unsupported erl_nif functions

  the `beam` import does not comprehensively provide support for all functions
  in `erl_nif.h`.  If you need access to a function in `erl_nif.h` that isn't
  provided by zigler, you would do it in the following fashion:

  - import `erl_nif` into your zig code, typically under the `e` namespace.
  - retrieve `beam.context.env` and use that as your ErlNifEnv pointer.
  - use `beam.term` for function return types, which is a struct with a single
    field, `v`, of type `ERL_NIF_TERM`.

  #### Example

  ```elixir
  defmodule WithErlNif do
    use Zig, otp_app: :zigler

    ~Z\"""
    const e = @import("erl_nif");
    const beam = @import("beam");

    pub fn add_one(number: u64) beam.term {
        return .{.v = e.enif_make_uint64(beam.context.env, number + 1)};
    }
    \"""
  end

  test "raw erl_nif_function" do
    assert 48 = WithErlNif.add_one(47)
  end
  ```

  > #### beam.context.env is a threadlocal {: .warning}
  >
  > `beam.context.env` is a threadlocal variable, and is not available when
  > calling functions using `raw` mode.  See [Raw](9-raw-mode.html) mode calling
  > for more information.

  ### Advanced usage: Manual marshalling

  If you need to marshal your own data, you may use the `beam.get` and
  `beam.make` functions to marshal data to and from the BEAM world.

  #### Example

  ```elixir
  defmodule ManualMarshalling do
    use Zig, otp_app: :zigler, nifs: [add_one: [spec: false]]

    @spec add_one(integer) :: integer

    ~Z\"""
    const beam = @import("beam");

    pub fn add_one(val: beam.term) !beam.term {
        const number = try beam.get(i64, val, .{});
        return beam.make(number + 1, .{});
    }
    \"""
  end

  test "manual marshalling" do
    assert 48 = ManualMarshalling.add_one(47)
  end
  ```

  For more details on [`get`](beam.html#get) and [`make`](beam.html#make)
  functions see the [`beam`](beam.html) documentation.

  > #### Manual Term marshalling {: .warning }
  >
  > If you don't use automatic marshalling, Zigler will not be able
  > to provide the following conveniences:
  >
  > - argument error details.  The zig code will raise a generic
  >   BEAM `ArgumentError` but it won't have specific details about
  >   what the expected type was and which argument was in error.
  >
  > - dialyzer type information for your function.  You will have
  >   to supply that type information outside `~Z` block, as shown
  >   in the example.

  ### Importing external files

  If you need to write zig code outside of the module, just place it in
  the same directory as your module.

  You may either call imported functions from the external file, or
  forward a function from the external file, either strategy will
  work correctly.

  #### Example

  ```zig
  \\\\ .extra_code.zig
  pub fn add_one(number: u64) u64 {
      return number + 1;
  }
  ```

  ```elixir
  defmodule ExternalImport do
    use Zig, otp_app: :zigler

    ~Z\"""
    const extra_code = @import(".extra_code.zig");
    pub fn add_one(number: u64) u64 {
        return extra_code.add_one(number);
    }

    pub const forwarded_add_one = extra_code.add_one;
    \"""
  end

  test "external imports by calling" do
    assert 48 = ExternalImport.add_one(47)
  end

  test "external imports by forwarding" do
    assert 48 = ExternalImport.forwarded_add_one(47)
  end
  ```

  ### Advanced Usage: Custom source location

  By default, Zigler places generated source code in the same directory
  as the module that uses Zigler, however, you may specify a different
  directory:

  ```elixir
  defmodule CustomSourceLocation do
    use Zig, otp_app: :zigler, dir: "test/.custom_location"

    ~Z\"""
    pub fn add_one(number: u64) u64 {
        return number + 1;
    }
    \"""
  end

  test "custom_location is built" do
    assert File.dir?("test/custom_location")
    assert File.exists?("test/.custom_location/.Elixir.CustomSourceLocation.zig")
  end
  ```

  ### Advanced usage: change staging directory location

  By default, zigler stages files in `/tmp/{modulename}` directory.  In some cases
  this will cause user collisions and permissions errors when trying to build modules
  on multitenant systems.  If you need to change the staging directory, set the
  `ZIGLER_STAGING_ROOT` environment variable to the desired directory.  The
  recommended staging directory is `~/.cache/zigler`.  NB: In the future, this
  may become the default staging directory.

  ### Other Environment Variables

  - `ZIG_ARCHIVE_PATH`: path to the directory where the zig compiler toolchain WAS downloaded.
    Expects an executable at: `ZIG_ARCHIVE_PATH/zig-<os>-<arch>-<version>/zig`.
  - `ZIG_EXECUTABLE_PATH`: direct path to the zig executable.
  - `ZIG_FMT`: if set to `false`, disables zig formatting steps.
  """

  @spec __using__(keyword) :: Macro.t()
  defmacro __using__(opts) do
    module = __CALLER__.module

    if :loaded == :code.module_status(module) do
      :code.purge(module)
    end

    if not Keyword.has_key?(opts, :otp_app) do
      raise CompileError,
        file: __CALLER__.file,
        line: __CALLER__.line,
        description:
          "(module #{inspect(module)}) you must supply an `otp_app` option to `use Zig`"
    end

    opts =
      opts
      |> Keyword.put(:language, Elixir)
      |> requote_use_opts

    Module.register_attribute(module, :zig_code_parts, accumulate: true)
    Module.register_attribute(module, :zig_code, persist: true)

    code =
      quote do
        @zigler_opts unquote(opts)

        import Zig, only: [sigil_Z: 2, sigil_z: 2]
        @on_load :__load_nifs__
        @before_compile Zig.Compiler
      end

    Zig.Macro.inspect(code, opts)
  end

  @typedoc """
  user options for the `use Zig` macro, or for the `zig_opts(...)` attribute in erlang.

  - `otp_app`: required.  Default location where the shared libraries will be installed depends
    on this value.
  - `c`: see `t:c_options/0` for details.
  - `optimize`: the release mode to use when building the shared object.
    - `:debug` (default) builds your shared object in zig's `Debug` build mode.
    - `:safe` builds your shared object in zig's `ReleaseSafe` build mode.
    - `:fast` builds your shared object in zig's `ReleaseFast` build mode.
    - `:small` builds your shared object in zig's `ReleaseSmall` build mode.
    - `:env` reads `ZIGLER_RELEASE_MODE` environment variable to determine the release mode.
    - `{:env, mode}` reads `ZIGLER_RELEASE_MODE` environment variable with fallback to the specified mode.
  - `easy_c`: path to a header file that will be used to generate a C wrapper.
    if this is set, you must specify `:nifs` without the `:auto` (or `...`) specifier.
    A path beginning with `./` will be treated as a relative to cwd (usually the project root),
    otherwise the path will be treated as relative to the module file.
    You may provide code using either the `c` > `link_lib` option or `c` > `src`. You
    may also NOT provide any `~Z` blocks in your module.
  - `zig_code_path`: path to a zig file that will be used to as a target.  A path beginning
    with `./` will be treated as relative to cwd (usually the project root), otherwise the
    path will be relative to the module file.  If you specify this option, you may NOT
    provide any `~Z` blocks in your module.
  - `nifs`: a list of nifs to be generated.  If you specify as `{:auto, nifs}`, zigler
    will search the target zig code for `pub` functions and generate the default nifs for
    those that do not appear in the nifs list.  If you specify as a list of nifs, only
    the nifs in the list will be used.  In Elixir, using `...` in your nifs list
    converts it to `{:auto, nifs}`.  The nifs list should be a keyword list with the
    keys being the function names.  See `t:nif_options/0` for details on the options.
  - `ignore`: any functions found in the `ignore` list will not be generated as nifs if
    you are autodetecting nifs.
  - `extra_modules`: a list of zig modules to be included in the build.  Each module is declared
    with a tuple of the form `{name, {path, deps}}` where `name` is the name of the module
    (as an atom), `path` is the path to the module, and `deps` is a list of transitive
    dependencies for that module.  Those dependencies must also be in the `extra_modules` list.
  - `resources`: a list of types in the zig code that are to be treated as resources.
  - `callbacks`: see `t:callback_option/0` for details.
  - `cleanup`: (default `true`) can be used to shut down cleanup for allocated datatypes
    module-wide.
  - `leak_check`: (default `false`) if set to `true`, by default all nifs will use the
    debug_allocator, and check for memory leaks at the end of each nif call.
  - `dump`: if set to `true`, the generated zig code will be dumped to the console.
  - `dump_sema`: if set to `true`, the semantic analysis of the generated zig code will be dumped to the console.
  - `dump_build_zig`: if set to `true`, the generated zig code will be dumped to the console.
    If set to `:stdout`, or `:stderr` it will be sent to the respective stdio channels. If set
    to a path, the generated zig code will be written to a file at that path.
  """
  @type options :: [
          otp_app: atom,
          c: [c_options],
          optimize: optimize | :env | {:env, optimize},
          easy_c: Path.t(),
          nifs: {:auto, keyword(nif_options)} | keyword(nif_options),
          ignore: [atom],
          module: [{name :: atom, {path :: Path.t(), deps :: [atom]}}],
          resources: [atom],
          callbacks: [callback_option],
          cleanup: boolean,
          leak_check: boolean,
          dump: boolean,
          dump_sema: boolean,
          dump_build_zig: boolean | :stdout | :stderr | Path.t()
        ]

  @type optimize :: :debug | :safe | :fast | :small

  @typedoc """
  options for compiling C code.  See `t:c_path/0` for details on how to specify paths.

  - `include_dirs`: a path or list of paths to search for C header files.
  - `library_dirs`: a path or list of paths to search for C libraries.
  - `link_lib`: a path or list of libraries to link against.
  - `link_libcpp`: if set to `true`, the C++ standard library will be linked.
  - `src`: a list of C source files to compile.  Each source file can be a tuple
    of the form `{path, options}` where `path` is the path to the source file and
    `options` is a list of compiler options to pass to the compiler when building
    the source file.  If no options are provided, the default options will be used.
  """
  @type c_options :: [
          include_dirs: c_path | [c_path],
          library_dirs: c_path | [c_path],
          link_lib: c_path | [c_path],
          link_libcpp: boolean,
          src: [c_path | {c_path, [compiler_options :: String.t()]}]
        ]

  @typedoc """
  Path specification for various C compilation options.  This may be:

  - a `t:Path.t/0` which is a relative path to the module file.  If the path begins
    with `./` it will be treated as a relative path to the current working directory.
  - `{:priv, path}` which is a relative path to the `priv` directory of `otp_app`.
  - `{:system, path}` which is an absolute path to the file.
    > ### System paths {: .warning}
    >
    > You should not use `{:system, path}` if you expect someone else to be building
    > the code.
  """
  @type c_path :: Path.t() | {:priv, Path.t()} | {:system, Path.t()}

  @typedoc """
  user options for individual nifs.

  - `export`: (default `true`) if `false`, the function will be private.
  - `concurrency`: the concurrency model to use.  See `t:concurrency/0` for options
    and [Nifs](https://www.erlang.org/doc/apps/erts/erl_nif.html) for details
    on their meanings.

    > ### Yielding {: .warning}
    >
    > Yielding nifs are not currently supported in Zigler but may return when
    > Async functions are again supported in Zig.

  - `spec`: (default `true`) if `false`, zigler will not generate a typespec
    for the function.  If used in conjuction with `@spec` you may provide a
    custom typespec for the function.
  - `allocator`: (default: `nil`) the allocator type to use for this function.
    if unset, the default allocator `beam.allocator` will be used.
    see [Allocators](03-allocators.html) for details on how to use allocators.
  - `params`: a map of parameter indices to lists of parameter options.  See
    `t:param_option/0` for details on the options.  Skipping paramater indices
    is allowed.
  - `return`: options for the return value of the function.  See `t:return_option/0`
    for details on the options.
  - `leak_check`: (default `false`) if set to `true`, the default allocator
    will be set to `std.heap.DebugAllocator` and the leak check method will
    be run at the end of the function.
  - `alias`: if set, the nif name will be the name of BEAM function in the
    module, but the zig function called will be the alias name.
  - `arity`: (only available for raw functions) the arities of the function
    that are accepted.
  - `impl`: sets the `@impl` attribute for the function.
  """
  @type nif_options :: [
          export: boolean,
          concurrency: concurrency,
          spec: boolean,
          allocator: nil | atom,
          params: integer | %{optional(integer) => [param_option]},
          return: as_type | [return_option],
          leak_check: boolean,
          alias: atom,
          arity: arity | Range.t(arity, arity) | [arity | Range.t(arity, arity)],
          impl: boolean | module
        ]

  @type concurrency :: :dirty_cpu | :dirty_io | :synchronous | :threaded | :yielding

  @typedoc """
  user options for nif parameters.

  - :noclean (same as `{:cleanup, false}`) will force the parameter to
    not be cleaned up after a function call.
  - :in_out (same as `{:in_out, true}`) will force the parameter to be
    an in-out parameter; the return value of the function will derive
    from this parameter's type instead of the return type.

    Only one parameter may be marked as `:in_out` in a function.
  - :sentinel (same as `{:sentinel, true}`) if the parameter is a `[*c]` type parameter,
    a sentinel should be attached when allocating space for the parameter.  This option is
    disallowed if the parameter is not a `[*c]`.
  """
  @type param_option ::
          :noclean | :in_out | {:cleanup, boolean} | {:in_out, boolean} | {:sentinel, boolean}

  @typedoc """
  user options for nif return values.

  - `:noclean` (same as `{:cleanup, false}`) will force the return value
    to not be cleaned up after a function call.
  - `:binary` same as `{:as, :binary}`
  - `:integer` same as `{:as, :integer}`
  - `:list` same as `{:as, :list}`
  - `:map` same as `{:as, :map}`
  - `:default` same as `{:as, :default}`
  - `{:error, atom}` (only for functions with in-out parameters) will
    convert the return value of the function to an error, by calling
    the function name.  Note this function must be `pub`.
  - `{:length, length}` specifies the length of the return value
    if it is a `[*]T`, or `[*c]T` type.  The length may be an integer
    or `{:arg, index}` if you would like the length to be specified
    by one of the parameters.
  - `{:struct, module}` coerces the return value to a struct of the
    given module.  This is only available for functions with struct returns.
  """
  @type return_option ::
          as_type
          | :noclean
          | {:cleanup, boolean}
          | {:as, as_type}
          | {:error, atom}
          | {:length, non_neg_integer | {:arg, non_neg_integer}}
          | {:struct, module}

  @typedoc """
  sets the return type of the function, if it's ambiguous.  For example,
  a `[]u8` can be forced to return a list instead of the default binary.

  For collections, you can specify deep typing.  For example`{:list, :list}`
  can be forced to return a list of lists for `[][]u8`.  Map fields can
  be set using a keyword list, for example `{:map, [foo: :list]}` will
  force a struct to return a map with the field `foo` typed as a list.
  """
  @type as_type ::
          :binary
          | :integer
          | :default
          | :list
          | :map
          | {:list, as_type}
          | {:map, keyword(as_type)}

  @typedoc """
  options for assigning hook functions to module management events.

  see [Module Callbacks](10-callbacks.html) for details on what function signatures are allowed
  for these callbacks.
  """
  @type callback_option ::
          :on_load
          | :on_upgrade
          | :on_unload
          | {:on_load, atom}
          | {:on_upgrade, atom}
          | {:on_unload, atom}

  @doc """
  declares a string block to be included in the module's .zig source file.
  """
  defmacro sigil_Z({:<<>>, meta, [zig_code]}, []) do
    quoted_code(zig_code, meta, __CALLER__)
  end

  @doc """
  like `sigil_Z/2`, but lets you interpolate values from the outside
  elixir context using string interpolation (the `\#{value}` form)
  """
  defmacro sigil_z(code = {:<<>>, _, _}, []) do
    quoted_code(code, [line: __CALLER__.line], __CALLER__)
  end

  defp quoted_code(zig_code, meta, caller) do
    opts = Module.get_attribute(caller.module, :zigler_opts)

    if opts[:easy_c] do
      raise CompileError,
        description: "you can't use ~Z in easy_c nifs",
        line: caller.line,
        file: caller.file
    end

    line = meta[:line]
    module = caller.module
    file = Path.relative_to_cwd(caller.file)

    quote bind_quoted: [module: module, zig_code: zig_code, file: file, line: line] do
      @zig_code_parts "// ref #{file}:#{line}\n"
      @zig_code_parts zig_code
      :nothing
    end
  end

  @doc """
  retrieves the zig code from any given module that was compiled with zigler
  """
  def code(module) do
    [code] = Keyword.fetch!(module.__info__(:attributes), :zig_code)
    code
  end

  @extension (case :os.type() do
                {:unix, :darwin} -> ".dylib"
                # linux, freebsd, openbsd
                {:unix, _} -> ".so"
                {_, :nt} -> ".dll"
              end)

  @doc """
  outputs a String name for the module.

  note that for filesystem use, you must supply the extension.  For internal (BEAM) use, the
  filesystem extension will be inferred.  Therefore we provide two versions of this function.
  """
  def nif_name(module, use_suffixes \\ true) do
    if use_suffixes do
      "lib#{module.module}#{@extension}"
    else
      "lib#{module.module}"
    end
  end

  @version Zigler.MixProject.zig_version()

  @doc """
  default version of zig supported by this version of zigler.

  > ### API warning {: .warning }
  >
  > this API may change in the future.
  """
  def version, do: @version

  # UTILITIES

  @doc false
  # implements the common path normalization scheme for files in the `use Zig`
  # directory:  "./<file>" maps to "project-relative, "/<file>" maps to "absolute"
  # and "<file>" maps to elixir module-relative.
  def _normalize_path(path, relative_dir) do
    case path do
      "./" <> rest ->
        Path.expand(rest)

      "/" <> _ ->
        path

      _ ->
        Path.expand(path, relative_dir)
    end
  end

  # converts `use Zig` options AST to a form that can be stored into a module
  # attribute.  Two `use Zig` features cannot be directly evaluated inside the
  # module.  First one is the `...` used to indicate to autodetect nifs.  Second
  # one is return: [spec: <typespec>] which uses typespec AST that can't be
  # evaluated by Elixir.

  defp requote_use_opts(ast) do
    Keyword.update(ast, :nifs, {:auto, []}, &requote_nifs/1)
  end

  defp requote_nifs(nifs_ast) do
    if Enum.any?(nifs_ast, &match?({:..., _, _}, &1)) do
      {:auto, requote_nifs_list(nifs_ast)}
    else
      requote_nifs_list(nifs_ast)
    end
  end

  defp requote_nifs_list(ast) do
    Enum.flat_map(ast, fn
      {:..., _, _} -> []
      {fun, opts} -> [{fun, requote_fun_opts(opts)}]
      fun when is_atom(fun) -> [{fun, []}]
    end)
  end

  defp requote_fun_opts(opts) do
    Enum.map(opts, fn
      {:return, opts} when is_list(opts) ->
        {:return, requote_return_opts(opts)}

      other ->
        other
    end)
  end

  defp requote_return_opts(opts) do
    Enum.map(opts, fn
      {:spec, spec} -> {:spec, Macro.escape(spec)}
      other -> other
    end)
  end

  case Code.ensure_loaded(JSON) do
    {:module, JSON} ->
      @doc false
      def _json_decode!(string), do: JSON.decode!(string)

      @doc false
      def _json_encode!(term, opts \\ []) do
        if Keyword.get(opts, :pretty, false) do
          term
          |> JSON.encode!()
          |> :json.format(%{indent: "  ", line_separator: "\n", after_colon: " "})
        else
          JSON.encode!(term)
        end
      end

    _ ->
      @doc false
      def _json_decode!(string), do: Jason.decode!(string)
      @doc false
      def _json_encode!(term, opts \\ []), do: Jason.encode!(term, opts)
  end

  @doc false
  # true if error return traces are available on this platform
  def _errors_available? do
    case :os.type() do
      {:unix, :darwin} ->
        # MacOS in general: https://github.com/ziglang/zig/issues/25433
        # x86, see: https://github.com/ziglang/zig/issues/25157
        false

      {:win32, :nt} ->
        # windows still causes panic? segfault? when unwinding error return traces.
        false

      _ ->
        true
    end
  end

  @doc false
  # Returns the system temporary directory with all symlinks resolved.
  # On macOS, /tmp and /var are symlinks to /private/tmp and /private/var,
  # which can cause issues with relative path resolution in Zig's build system.
  def _tmp_dir do
    tmp = System.tmp_dir()
    # Remove trailing slash to normalize the path
    tmp = String.trim_trailing(tmp, "/")
    resolve_symlinks(tmp)
  end

  # Recursively resolve all symlinks in a path by checking each component
  defp resolve_symlinks(path) do
    # Split the path into components
    parts = Path.split(path)

    # Rebuild the path, resolving symlinks at each level
    {resolved, _} =
      Enum.reduce(parts, {"", ""}, fn part, {current_path, _} ->
        next_path =
          if current_path == "" do
            part
          else
            Path.join(current_path, part)
          end

        # Check if this component is a symlink
        case File.read_link(next_path) do
          {:ok, link_target} ->
            # If the link target is absolute, use it directly
            # Otherwise, resolve it relative to the current directory
            resolved_path =
              if String.starts_with?(link_target, "/") do
                link_target
              else
                Path.join(Path.dirname(next_path), link_target)
              end

            {resolved_path, ""}

          {:error, _} ->
            # Not a symlink, continue with the current path
            {next_path, ""}
        end
      end)

    resolved
  end
end

# check that the otp_version is 24 or greater.
otp_version =
  :otp_release
  |> :erlang.system_info()
  |> List.to_integer()

if otp_version < 24 do
  raise CompileError, description: "zigler requires OTP 24 or greater"
end
