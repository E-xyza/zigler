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
    In fact, Zig is likely to be an easier glue language for C ABIs than
    C.

  ### Basic usage

  In the BEAM, you can define a NIF by consulting the following [document](
  https://erlang.org/doc/man/erl_nif.html) and implementing the appropriate
  shared object/DLL callbacks.  However, Zigler will take care of all of
  this for you.

  Simply `use Zig` in your module, providing the app atom in the property
  list.

  Then, use the `sigil_Z/2` macro and write zig code.  Any nifs you define
  should be preceded with the `/// nif: function_name/arity` zig docstring.

  #### Example
  ```
  defmodule MyModule do
    use Zig

    ~Z\"""
    /// nif: my_func/1
    fn my_func(val: i64) i64 {
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
  the `beam.term` type which is equivalent to the `ERLNIFTERM` type.  See
  [`erl_nif`](erl_nif.html).

  ### Guides

  Please consult the following guides for detail topics:

  - [different execution modes](nifs.html)
  - [how to build BEAM `resources`](resources.html)

  ### Nerves Support

  Nerves is supported out of the box, and the system should cross-compile
  to arm ABI as necessary depending on what your nerves `:target` is.  You
  may also directly specify a zig target using the
  `use Zig, target: <target>` option.

  ### Environment

  Sometimes, you will need to pass the BEAM environment (which is the code
  execution context, including process info, etc.) into the NIF function.  In
  this case, you should pass it as the first argument, as a `beam.env` type
  value.

  #### Example

  ```
  defmodule MyModule do
    use Zig

    ~Z\"""
    /// nif: my_func_with_env/1
    fn my_func_with_env(env: beam.env, pid: beam.pid) void {
      var sendable_term: []u64 = "ping"[0..];
      var msg = beam.make_slice(env, sendable_term);
      var res = e.enif_send(env, pid, env, msg);
    }
    \"""
  end
  ```

  ### Bring your own version of Zig

  If you would like to use your system's local `zig` command, set the
  `local_zig` option in `config.exs`, which

  ```
  config :zigler, local_zig: true
  ```

  This will use `System.find_executable` to obtain the zig command. If
  you want to specify the zig command manually, use the following:

  ```
  config :zigler, local_zig: "path/to/zig/command"
  ```

  Note that for minor versions prior to 1.0, zigler doesn't plan on
  maintaining backward compatibility due to large architectural changes.

  ### External Libraries

  If you need to bind static (`*.a`) or dynamic (`*.so`) libraries into your
  module, you may link them with the `:libs` argument.

  Note that zig statically binds shared libraries into the assets it creates.
  This simplifies deployment for you.

  #### Example (explicit library path)

  ```
  defmodule Blas do
    use Zig,
      libs: ["/usr/lib/x86_64-linux-gnu/blas/libblas.so"],
      include: ["/usr/include/x86_64-linux-gnu"]

    ~Z\"""
    const blas = @cImport({
      @cInclude("cblas.h");
    ...
  ```

  You can also link system libraries.  This relies on `zig build`'s ability
  to locate system libraries.  Note that you will need to follow your system's
  library convention, for example in the case of linux, that means removing the
  "lib" prefix and the ".so" extension.

  #### Example (system libraries)

  ```
  defmodule Blas do
    use Zig,
      system_libs: ["blas"],
      include: ["/usr/include/x86_64-linux-gnu"]

    ~Z\"""
    const blas = @cImport({
      @cInclude("cblas.h");
    ...
  ```

  ### Compiling C/C++ files

  You can direct zigler to use zig cc to compile C or C++ files that are in
  your directory tree.  Currently, you must explicitly pick each file, in the
  future, there may be support for directories (and selecting compile options)
  based on customizeable rules.

  To do this, fill the "sources" option with a list of files (represented as
  strings), or a file/options pair (represented as a tuple).

  ```
  defmodule UsesCOrCpp do
    use Zig,
      link_libc: true,
      link_libcpp: true,
      include: ["my_header.h"],
      sources: [
        "some_c_source.c",
        {"some_cpp_source.cpp", ["-std=c++17"]}
      ]

    ~Z\"""
    ...
  ```

  Don't forget to include relevant h files, and set the `link_libc: true`
  and/or the `link_libcpp: true` options if your code needs the c or c++
  standard libraries

  ### Compilation assistance

  If something should go wrong, Zigler will translate the Zig compiler error
  into an Elixir compiler error, and let you know exactly which line in the
  `~Z` block it came from.

  ### Syntactic Sugar

  Some of the erlang nif terms can get unwieldy, especially in Zig, which
  prefers terseness.  Each of the basic BEAM types is shadowed by a Zig type
  in the `beam` module.  The `beam` struct is always imported into the header
  of the zig file used, so all zig code in the same directory as the module
  should have access to the `beam` struct if they `@import("beam.zig")`

  ### Importing files

  If you need to write code outside of the basic module (you will, for anything
  non-trivial), just place it in the same directory as your module.

  #### Example

  ```
  ~Z\"""
  const extra_code = @import("extra_code.zig");

  /// nif: use_extra_code/1
  fn use_extra_code(val: i64) i64 {
    return extra_code.extra_fn(val);
  }
  \"""
  ```

  If you would like to include a custom c header file, create an `include/`
  directory inside your path tree and it will be available to zig as a default
  search path as follows:

  ```
  ~Z\"""
  const c = @cImport({
    @cInclude("my_c_header.h");
  });

  // nif: my_nif/1
  ...
  \"""
  ```

  If the c header defines `extern` functions, it's your responsibility to make
  sure those externed functions are available by compiling other c files or
  using a shared library.

  ### Documentation

  Use the builtin zig `///` docstring to write your documentation.  If it's in
  front of the nif declaration, it will wind up in the correct place in your
  elixir documentation.

  See `Zig.Doc` for more information on how to document in zig and what to
  document.  See `Mix.Tasks.ZigDoc` for information on how to get your Elixir
  project to incorporate zig documentation.

  ### Tests

  Use the builtin zig `test` keyword to write your internal zig unit tests.
  These can be imported into an ExUnit module by following this example:

  ```
  defmodule MyTest do
    use ExUnit.Case
    use Zig.Unit
    zigtest ModuleWithZigCode
  end
  ```

  See `Zig.Unit` for more information.

  """

  alias Zig.Compiler
  alias Zig.Parser

  # default release modes.
  # you can override these in your `use Zigler` statement.
  @spec __using__(keyword) :: Macro.t()
  defmacro __using__(opts) do
    # mode = opts[:release_mode] || @default_release_mode

    # clear out the assembly directory
    Mix.env()
    |> Compiler.assembly_dir(__CALLER__.module)
    |> File.rm_rf!()

    user_opts =
      opts
      |> Keyword.take(~w(libs resources dry_run c_includes system_include_dirs
        local link_libc link_libcpp sources system_libs)a)

    include_dirs =
      opts
      |> Keyword.get(:include, [])
      |> Kernel.++(if has_include_dir?(__CALLER__), do: ["include"], else: [])

    zigler! =
      struct(
        %Zig.Module{
          file: Path.relative_to_cwd(__CALLER__.file),
          module: __CALLER__.module,
          imports: Zig.Module.imports(opts[:imports]),
          include_dirs: include_dirs,
          version: get_project_version(),
          otp_app: get_app()
        },
        user_opts
      )

    zigler! = %{zigler! | code: Zig.Code.headers(zigler!)}

    Module.register_attribute(__CALLER__.module, :zigler, persist: true)
    Module.put_attribute(__CALLER__.module, :zigler, zigler!)

    quote do
      import Zig
      require Zig.Compiler

      @on_load :__load_nifs__

      @before_compile Zig.Compiler
    end
  end

  defp has_include_dir?(env) do
    env.file
    |> Path.dirname()
    |> Path.join("include")
    |> File.dir?()
  end

  @doc """
  declares a string block to be included in the module's .zig source
  file.  At least one of these blocks must define a nif.
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
    line = meta[:line]
    module = caller.module
    file = Path.relative_to_cwd(caller.file)

    quote bind_quoted: [module: module, zig_code: zig_code, file: file, line: line] do
      zigler = Module.get_attribute(module, :zigler)

      new_zigler =
        zig_code
        |> Parser.parse(zigler, file, line)

      @zigler new_zigler
    end
  end

  defp get_project_version do
    Mix.Project.get()
    |> apply(:project, [])
    |> Keyword.get(:version)
    |> Version.parse!()
  end

  defp get_app do
    Mix.Project.get()
    |> apply(:project, [])
    |> Keyword.get(:app)
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
end
