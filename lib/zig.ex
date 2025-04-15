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

  > ### Zig version support {: .warning }
  >
  > although the large-scale archictecture of zigler is settled,
  > zigler features may break backwards compatibility until zig reaches
  > 1.0

  ### Nerves Support

  Nerves is supported out of the box, and Zigler will be able to seamlessly
  detect the cross-compilation information (os, architecture, runtime) and
  build correctly for that target.

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

  alias Zig.Options

  @spec __using__(keyword) :: Macro.t()
  defmacro __using__(opts) do
    module = __CALLER__.module

    if :loaded == :code.module_status(module) do
      :code.purge(module)
    end

    if not Keyword.has_key?(opts, :otp_app) do
      raise CompileError, file: __CALLER__.file, line: __CALLER__.line,
        description: "(module #{inspect module}) you must supply an `otp_app` option to `use Zig`"
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

  @version Zigler.MixProject.zig_version()

  @doc """
  default version of zig supported by this version of zigler.

  > ### API warning {: .warning }
  >
  > this API may change in the future.
  """
  def version, do: @version

  # UTILITIES

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
      {:return, opts} when is_list(opts)-> 
        {:return, requote_return_opts(opts)}
      other -> other
    end)
  end

  defp requote_return_opts(opts) do
    Enum.map(opts, fn
      {:spec, spec} -> {:spec, Macro.escape(spec)}
      other -> other
    end)
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
