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
  extra_code = @import("extra_code.zig");

  /// nif: use_extra_code/1
  fn use_extra_code(val: i64) i64 {
    return extra_code.extra_fn(val);
  }
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

  @doc """
  queries the zig website to obtain the latest version of zig.  Only performed at compile time.
  """
  defmacro latest_version() do
    Application.ensure_all_started(:ssl)
    # find the latest version by querying the download index
    case Mojito.get("https://ziglang.org/download/index.json",[], pool: false, timeout: 100_000) do
      {:ok, %{status_code: 200, body: json}} ->
        json
        |> Jason.decode!
        |> Map.keys
        |> Enum.reject(&(&1 == "master"))
        |> Enum.map(&String.split(&1, "."))
        |> Enum.map(&List.to_tuple/1)
        |> Enum.sort
        |> List.last
        |> Tuple.to_list
        |> Enum.join(".")
      _ -> Mix.raise("failed to ascertain the latest version of zig.")
    end
  end

  # default release modes.
  # you can override these in your `use Zigler` statement.
  @default_release_modes %{prod: :safe, dev: :debug, test: :debug}
  @default_release_mode @default_release_mode[Mix.env()]

  defmacro __using__(opts) do
    unless opts[:app] do
      raise ArgumentError, "you must provide the application"
    end

    mode = opts[:release_mode] || @default_release_mode

    # make sure that we're in the correct operating system.
    if match?({:win32, _}, :os.type()) do
      raise "non-unix systems not currently supported."
    end

    mod_path =  opts[:app]
    |> Application.app_dir("priv/nifs")
    |> Path.join(Macro.underscore(__CALLER__.module))

    zig_version = opts[:version] || latest_version()

    File.mkdir_p!(Path.dirname(mod_path))

    src_dir = Path.dirname(__CALLER__.file)

    quote do
      import Zigler

      @release_mode unquote(mode)

      @on_load :__load_nifs__
      @zigler_app unquote(opts[:app])
      @zig_version unquote(zig_version)

      # needs to be persisted so that we can store the version for tests.
      Module.register_attribute(__MODULE__, :zigler_app, persist: true)
      Module.register_attribute(__MODULE__, :zig_version, persist: true)
      Module.register_attribute(__MODULE__, :zig_specs, accumulate: true)
      Module.register_attribute(__MODULE__, :zig_code, accumulate: true, persist: true)
      Module.register_attribute(__MODULE__, :zig_imports, accumulate: true)
      Module.register_attribute(__MODULE__, :zig_src_dir, persist: true)

      @zig_src_dir unquote(src_dir)

      @before_compile Zigler.Compiler
    end
  end

  @doc """
  Analyzes Zig code inline, then assembles a series of code files, stashes them in a
  temporary directory, compiles it with NIF adapters, and then binds it into the current
  module.  You may have multiple sigil_Z blocks in a single Elixir module if you wish.
  """
  defmacro sigil_Z({:<<>>, meta, [zig_code]}, []) do
    file = __CALLER__.file
    line = meta[:line]

    # perform code analysis
    code = Zigler.Code.from_string(zig_code, file, line)

    # add a specs list to be retrieved by the compiler.
    code_spec = Enum.map(code.nifs, &{&1.name, {&1.params, &1.retval}})

    empty_functions = Enum.flat_map(code.nifs, fn nif ->
      if nif.doc do
        [{:@,
           [context: Elixir, import: Kernel],
           [{:doc, [context: Elixir], [IO.iodata_to_binary(nif.doc)]}]}]
      else
        []
      end
      ++
      [
        typespec_for(nif),
        empty_function(nif.name, nif.arity),
      ]
    end)

    quote do
      @zig_code unquote(code.code)
      @zig_specs unquote(code_spec)
      unquote_splicing(empty_functions)
    end
  end

  @doc false
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

  @type_for %{
    "c_int" => :integer, "c_long" => :integer, "isize" => :integer,
    "usize" => :integer, "u8" => :integer, "i32" => :integer, "i64" => :integer,
    "f16" => :float, "f32" => :float, "f64" => :float,
    "beam.term" => :term, "e.ErlNifTerm" => :term,
    "beam.pid" => :pid, "e.ErlNifPid" => :pid,
    "beam.binary" => :binary, "e.ErlNifBinary" => :binary,
    "[]u8" => :binary
  }

  @doc false
  def typespec_for(%{name: name, params: params, retval: retval}) do
    [ret_tag] = type_for(retval)
    {:@, [context: Elixir, import: Kernel], [
      {:spec, [context: Elixir],
       [
         {:"::", [],
          [
            {name, [], Enum.flat_map(params, &type_for/1)},
            ret_tag
          ]}
       ]}
    ]}
  end

  @doc false
  def type_for("?*e.ErlNifEnv"), do: []
  def type_for("beam.env"), do: []
  def type_for("[]" <> val) when val != "u8", do: [type_for(val)]
  def type_for(" " <> val), do: type_for(val)
  def type_for(zig_type) do
    [{@type_for[zig_type], [], Elixir}]
  end

end
