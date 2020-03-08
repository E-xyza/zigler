defmodule Zigler do

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
  - OOM safety and sanity.  Zig makes it easy to defer memory destruction,
    meaning you often don't have to worry about memory leaks.  By design,
    `alloc` should be failable (as is the BEAM's `alloc`) so it offers
    an extra layer of protection for you BEAM VM against an OOM situation.

  ### Basic usage

  In the BEAM, you can define a NIF by consulting the following [document](
  https://erlang.org/doc/man/erl_nif.html) and implementing the appropriate
  shared object/DLL callbacks.  However, Zigler will take care of all of
  this for you.

  Simply `use Zigler` in your module, providing the app atom in the property
  list.

  Then, use the `sigil_Z/2` macro and write zig code.  Any nifs you define
  should be preceded with the `/// nif: function_name/arity` zig docstring.

  #### Example
  ```
  defmodule MyModule do
    use Zigler, app: :my_app

    ~Z\"""
    /// nif: my_func/1
    fn my_func(val: i64) i64 {
      return val + 1;
    }
    \"""

  end
  ```

  Zigler will *automatically* fill out the appropriate NIF C template, compile
  the shared object, and bind it into the module pre-compilation. In the case
  of the example, there will be a `MyModule.my_func/1` function call found in
  the module.

  Zigler will also make sure that your statically-typed Zig data are guarded
  when you marshal it from the dynamically-typed BEAM world.  However, you may
  only pass in and return certain types (though the generic `beam.term` type)
  is supported.

  ### Environment

  Sometimes, you will need to pass the BEAM environment (which is the code
  execution context, including process info, etc.) into the NIF function.  In
  this case, you should pass it as the first parameter, as a `beam.env` type
  value.

  #### Example

  ```
  defmodule MyModule do
    use Zigler, app: :my_app

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

  ### External Libraries

  If you need to bind static (`*.a`) or dynamic (`*.so`) libraries into your
  module, you may link them with the `:libs` parameter.

  Note that for shared libraries, a library with an identical path must exist
  in the target release environment.

  #### Example

  ```
  defmodule Blas do
    use Zigler,
      app: :my_app,
      libs: ["/usr/lib/x86_64-linux-gnu/blas/libblas.so"],
      include: ["/usr/include/x86_64-linux-gnu"]

    ~Z\"""
    const blas = @cImport({
      @cInclude("cblas.h");
    ...
  ```

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

  ### Documentation

  Use the builtin zig `///` docstring to write your documentation.  If it's in
  front of the nif declaration, it will wind up in the correct place in your
  elixir documentation.

  See `Zigler.Doc` for more information on how to document in zig and what to
  document.  See `Mix.Tasks.ZigDoc` for information on how to get your Elixir
  project to incorporate zig documentation.

  ### Tests

  Use the builtin zig `test` keyword to write your internal zig unit tests.
  These can be imported into an ExUnit module by following this example:

  ```
  defmodule MyTest do
    use ExUnit.Case
    use Zigler.Unit
    zigtest ModuleWithZigCode
  end
  ```

  See `Zigler.Unit` for more information.

  """

  alias Zigler.Parser

  # default release modes.
  # you can override these in your `use Zigler` statement.
  @default_release_modes %{prod: :safe, dev: :debug, test: :debug}
  @default_release_mode @default_release_modes[Mix.env()]

  defmacro __using__(opts) do
    mode = opts[:release_mode] || @default_release_mode

    # make sure that we're in the correct operating system.
    if match?({:win32, _}, :os.type()) do
      raise "non-unix systems not currently supported."
    end

    user_opts = Keyword.take(opts, [:libs, :resources, :dry_run])

    zigler = struct(%Zigler.Module{
      file:   __CALLER__.file,
      module: __CALLER__.module,
      semver: get_semver(),
      app:    get_app()}, user_opts)

    Module.register_attribute(__CALLER__.module, :zigler, persist: true)
    Module.put_attribute(__CALLER__.module, :zigler, zigler)

    quote do
      import Zigler
      require Zigler.Compiler

      @on_load :__load_nifs__

      @before_compile Zigler.Compiler
    end
  end

  @doc """
  Parses zig code and then accumulates it into the module's :zigler attribute.
  Doesn't actually write any code, since it can all be taken care of in the
  `Zigler.Compiler__before_compile__/1` directive.
  """
  defmacro sigil_Z({:<<>>, meta, zig_code}, []) do
    line = meta[:line]

    zigler = Module.get_attribute(__CALLER__.module, :zigler)

    new_zigler = zig_code
    |> IO.iodata_to_binary
    |> Parser.parse(zigler, line)

    Module.put_attribute(__CALLER__.module, :zigler, new_zigler)
    quote do end
  end

  @spec dunderize(String.t | integer) :: atom
  defp dunderize(string), do: String.to_atom("__#{string}__")

  @spec launch_func_name(atom) :: atom
  defp launch_func_name(func) do
    func
    |> Atom.to_string
    |> Kernel.<>("_launch")
    |> dunderize
  end

  @spec fetch_func_name(atom) :: atom
  defp fetch_func_name(func) do
    func
    |> Atom.to_string
    |> Kernel.<>("_fetch")
    |> dunderize
  end

  defp launch_call(func, arity) do
    {launch_func_name(func), [], for idx <- 1..arity do
      {dunderize(idx), [], Elixir}
    end}
  end

  def launch_function(func, arity) do
    launch_func = launch_func_name(func)
    {:defp, [context: Elixir, import: Kernel],
      [
        {launch_func, [context: Elixir], for _idx <- 1..arity do {:_, [], Elixir} end},
        [do: {:throw, [context: Elixir, import: Kernel],
        ["#{launch_func}/#{arity} not defined"]}]
      ]}
  end

  def empty_function(func, 0) do
    quote do
      def unquote(func)(), do: throw unquote("#{func}/0 not defined")
    end
  end

  def empty_function(func, arity) do
    {:def, [context: Elixir, import: Kernel],
    [
      {func, [context: Elixir], for _ <- 1..arity do {:_, [], Elixir} end},
      [
        do: {:throw, [context: Elixir, import: Kernel],
         ["#{func}/#{arity} not defined"]}
      ]
    ]}
  end

  @zig_dir_path Path.expand("../zig", Path.dirname(__ENV__.file))

  def latest_cached_zig_version do
    @zig_dir_path
    |> File.ls!
    |> Enum.filter(&match?("zig" <> _, &1))
    |> Enum.filter(&File.dir?(Path.join(@zig_dir_path, &1)))
    |> Enum.map(&String.split(&1, "-"))
    |> Enum.map(fn ["zig", _os, _arch, ver] -> ver end)
    |> Enum.map(&String.split(&1, "."))
    |> Enum.map(&List.to_tuple/1)
    |> case do
      [] ->
        raise CompileError, description: "no zig binaries found, run `mix zigler.get_zig latest`"
      lst ->
        lst
        |> Enum.sort
        |> List.last
        |> Tuple.to_list
        |> Enum.join(".")
    end
  rescue
    _err in FileError ->
      reraise CompileError, description: "zig directory path doesn't exist, run `mix zigler.get_zig latest`"
  end

  defp get_semver do
    Mix.Project.get
    |> apply(:project, [])
    |> Keyword.get(:version)
    |> String.split(".")
  end

  defp get_app do
    Mix.Project.get
    |> apply(:project, [])
    |> Keyword.get(:app)
  end

  def nif_dir(app \\ get_app()) do
    app
    |> :code.lib_dir
    |> Path.join("nif")
  end

  def nif_name(module, use_suffixes \\ true) do
    if use_suffixes do
      [major, minor, patch] = module.semver
      "lib#{module.module}.so.#{major}.#{minor}.#{patch}"
    else
      "lib#{module.module}"
    end
  end

end
