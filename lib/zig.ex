defmodule Zig do
  @moduledoc """

  Inline NIF support for [Zig](https://ziglang.org)

  ### Motivation

  > Zig is a general-purpose programming language designed for robustness,
  > optimality, and maintainability.

  The programming philosophy of Zig matches up nicely with the programming
  philosophy of the BEAM VM and in particular its emphasis on simplicity and
  structure should very appealing to the practitioners of Elixir.

  The following features make Zig extremely amenable to inline language
  support in a BEAM language:

  - simplicity.  Zig's syntax is definable in a simple YACC document and
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

  - [Using Nifs](1-nifs.html)
  - [Collection datatypes](2-collections.html)
  - [Allocator strategies](3-allocators.html)
  - [Nif options](4-nif_options.html)
  - [Resources](5-nif_options.html)
  - [C integration](6-c_integration.html)
  - [Concurrency strategies](7-concurrency.html)
  - [Global module options](8-module_options.html)

  > ### Zig version support {: .warning }
  >
  > although the large-scale archictecture of zigler is settled,
  > zigler features may break backwards compatibility until zig reaches
  > 1.0

  ### Nerves Support

  Nerves is supported out of the box, and Zigler will be able to seamlessly
  detect the cross-compilation information (os, architecture, runtime) and
  build correctly for that target.

  ### Basic usage

  In the BEAM, you can define a NIF by consulting the following [document](
  https://erlang.org/doc/man/erl_nif.html) and implementing the appropriate
  shared object/DLL callbacks.  However, Zigler will take care of all of
  this for you.

  Simply `use Zig` in your module, providing the app atom in the property
  list.

  Then, use the `sigil_Z/2` macro and write zig code.  To present a function
  as a nif in your module, simply export it from your code namespace by
  making it a `pub` function in your zig code.

  #### Example
  ```
  defmodule MyModule do
    use Zig, otp_app: :my_app

    ~Z\"""
    pub fn my_func(val: i64) i64 {
        return val + 1;
    }
    \"""

  end
  ```

  Zig will *automatically* fill out the appropriate NIF C template, compile
  the shared object, and bind it into the module pre-compilation. In the case
  of the example, there will be a `MyModule.my_func/1` function call found in
  the module.

  Zig will also make sure that your statically-typed Zig data are guarded
  when you marshal it from the dynamically-typed BEAM world.  However, you may
  only pass in and return certain types.  As an escape hatch, you may use
  the [`beam.term`](beam.html#term) type which is a wrapped
  [`ERL_NIF_TERM`](https://www.erlang.org/doc/man/erl_nif.html#ERL_NIF_TERM) type.
  See [`erl_nif`](https://www.erlang.org/doc/man/erl_nif.html).

  #### Environment

  For many functions, you'll need to import the [`beam`](beam.html) package and
  create a function that takes a [`beam.env`](beam.html#env) as its first
  argument.  This will enable you to directly access or create wrapped beam
  term data.  The equivalent of the above code will be:

  #### Example

  ```
  defmodule MyModule do
    use Zig, otp_app: :my_app

    ~Z\"""
    const beam = @import("beam");

    pub fn my_func(val_term: beam.term) !beam.term {
        const val = try beam.get(i64, val_term, .{});
        return beam.make(val + 1, .{});
    }
    \"""
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
  >   to supply that type information in your nif configuration.

  ### Functions missing from [`beam`](beam.html)

  The `beam` module doesn't comprehensively contain all nif functions.
  For functions that correspond to [`erl_nif.h`](https://www.erlang.org/doc/man/erl_nif.html)
  you can import the erl_nif package, which has the `erl_nif` C API

  ```zig
  const erl_nif = @import("erl_nif");
  ```

  ### Importing external files

  If you need to write zig code outside of the module, just place it in
  the same directory as your module.

  #### Example

  ```elixir
  ~Z\"""
  const extra_code = @import("extra_code.zig");

  pub fn use_extra_code(val: i64) i64 {
    return extra_code.extra_fn(val);
  }

  pub const forwarded_function = extra_code.forwarded_function;
  \"""
  ```

  If you would like to include a custom c header file, create an subdirectory
  of your module's directory and add it as an available include directory,
  as shown here (in this case the subdirectory is called `include`).  The
  Zig build system will add the include path(s) in the analysis and
  compilation pipelines.

  ```elixir
  defmodule MyModule
    use Zig,
      otp_app: :my_app,
      include_dir: "include"

    ~Z\"""
    const c = @cImport({
      @cInclude("my_c_header.h");
    });
    ...
    \"""
  end
  ```

  If the c header defines `extern` functions, it's your responsibility to make
  sure those externed functions are available by
  [compiling other c files](#module-compiling-c-c-files) or
  [using an external library](#module-external-libraries).

  ### External Libraries

  If you need to bind static (`*.a`) or dynamic (`*.so`) libraries into your
  module, you may link them with the `:libs` argument.

  Note that zig statically binds shared libraries into the assets it creates.
  This simplifies deployment for you.

  #### Example (explicit library path)

  ```elixir
  defmodule Blas do
    use Zig,
      otp_app: :my_app,
      link_lib: "path/to/libblas.a"

    ~Z\"""
    const blas = @cImport({
      @cInclude("cblas.h");
    ...
    \"""
  end
  ```

  You can also link system libraries.  This relies on `zig build`'s ability
  to locate system libraries.  Note that you will need to follow your system's
  library convention, for example in the case of linux, that means removing the
  "lib" prefix and the ".so" extension.

  #### Example (system libraries)

  ```elixir
  defmodule Blas do
    use Zig,
      otp_app: :my_app,
      link_lib: {:system, "blas"}

    ~Z\"""
    const blas = @cImport({
      @cInclude("cblas.h");
    ...
    \"""
  end
  ```

  ### Compiling C/C++ files

  You can direct zigler to compile C or C++ files that are in
  your directory tree.  Currently, you must explicitly pick each file, in the
  future, there may be support for directories (and selecting compile options)
  based on customizeable rules.

  To do this, fill the "sources" option with a list of files (represented as
  strings), or a file/options pair (represented as a tuple).

  ```elixir
  defmodule UsesCOrCpp do
    use Zig,
      otp_app: :my_app,
      link_libcpp: true,  # note: optional for c-only code
      include_dir: ["include"],
      c_src: [
        "some_c_source.c",
        {"some_cpp_source.cpp", ["-std=c++17"]},
        {"directory_of_files/*", ["-std=c99"]},
      ]

    ~Z\"""
    ...
    \"""
  end
  ```

  ### Easy C

  In some cases, you may have a C project that ships with a library and a
  header file that you would like to mount as NIF functions in your module.
  In this case, you can use the `easy_c` option to automate the work of
  stitching your library into the module.  Note that in this case, you must
  declare all of the function that you would like to import.  Here is an
  example of importing three functions from the blas example as above.

  For details of what the nif options mean, see: `Zig.EasyC`

  ```elixir
  defmodule BlasWithEasyC do
    use Zig,
      otp_app: :my_app,
      easy_c: "cblas.h",
      link_lib: {:system, "blas"},
      nifs: [
        :cblas_dasum,
        cblas_daxpy: [return: [4, length: {:arg, 0}]],
        daxpy_bin: [alias: :cblas_daxpy, return: [4, :binary, length: {:arg, 0}]]
      ]
  end
  ```

  ### Compilation debug

  If something should go wrong, Zigler will translate the Zig compiler error
  into an Elixir compiler error, and let you know which line in the
  `~Z` block it came from.

  ### Documentation

  Use the builtin zig `///` docstring to write your documentation.  If it's in
  front of the nif declaration, it will wind up in the correct place in your
  elixir documentation.

  Note that the `//!` docstring is not supported.  Use `@moduledoc` instead.

  ### Bring your own version of Zig

  If you would like to use your system's local `zig` command, specify
  this in your `use Zig` statement options.

  ```elixir
  use Zig, otp_app: :my_app, local_zig: true
  ```

  This will use `System.find_executable/1` to obtain the zig command. If
  you want to specify a specific zig path, use the following:

  ```elixir
  use Zig, otp_app: :my_app, zig_path: "path/to/zig/command"
  ```

  """

  alias Zig.Compiler
  alias Zig.Options
  # default release modes.
  # you can override these in your `use Zigler` statement.
  @spec __using__(keyword) :: Macro.t()
  defmacro __using__(opts) do
    module = __CALLER__.module
    if module in :erlang.loaded(), do: :code.purge(module)

    opts =
      opts
      |> Keyword.merge(mod_file: __CALLER__.file, mod_line: __CALLER__.line)
      |> Options.elixir_normalize!()

    # TODO: check to make sure the otp_app exists
    case Keyword.fetch(opts, :otp_app) do
      {:ok, _app} ->
        :ok

      _ ->
        raise CompileError,
          description: "you must supply the otp_app for the nifs",
          file: __CALLER__.file,
          line: __CALLER__.line
    end

    # clear out the assembly directory
    # TODO: make sure this is accessible.
    Mix.env()
    |> Compiler.assembly_dir(module)
    |> File.rm_rf!()

    Module.register_attribute(module, :zig_code_parts, accumulate: true)
    Module.register_attribute(module, :zig_code, persist: true)

    code =
      quote do
        @zigler_opts unquote(opts)

        import Zig, only: [sigil_Z: 2, sigil_z: 2]
        @on_load :__load_nifs__
        @before_compile Zig.Compiler
        @zig_code_parts [
          "// this code is autogenerated, do not check it into to your code repository\n\n"
        ]
      end

    Zig.Macro.inspect(code, opts)
  end

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
                {:unix, :linux} -> ".so"
                {:unix, :freebsd} -> ".so"
                {:unix, :darwin} -> ".dylib"
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

  @doc """
  default version of zig supported by this version of zigler.

  > ### API warning {: .warning }
  >
  > this API may change in the future.
  """
  def version, do: "0.11.0"
end
