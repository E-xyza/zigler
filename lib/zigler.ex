defmodule Zigler do

  @latest_zig_version Application.get_env(:zigler, :latest_zig_version)

  defmacro __using__(opts) do
    unless opts[:app] do
      raise ArgumentError, "you must provide the application"
    end

    mode = opts[:release_mode] || Application.get_env(:zigler, :release_mode)

    # make sure that we're in the correct operating system.
    if match?({:win32, _}, :os.type()) do
      raise "non-unix systems not currently supported."
    end

    mod_path =  opts[:app]
    |> Application.app_dir("priv/nifs")
    |> Path.join(Macro.underscore(__CALLER__.module))

    zig_version = opts[:version] || @latest_zig_version

    File.mkdir_p!(Path.dirname(mod_path))

    quote do
      import Zigler

      @release_mode unquote(mode)

      # needs to be persisted so that we can store the version for tests.
      Module.register_attribute(__MODULE__, :zig_version, persist: true)

      @on_load :__load_nifs__
      @zigler_app unquote(opts[:app])
      @zig_version unquote(zig_version)

      def __load_nifs__ do
        unquote(mod_path)
        |> String.to_charlist()
        |> :erlang.load_nif(0)
      end

      Module.register_attribute(__MODULE__, :zig_specs, accumulate: true)
      Module.register_attribute(__MODULE__, :zig_code, accumulate: true)

      @before_compile Zigler.Compiler
    end
  end

  alias Zigler.Code
  alias Zigler.Parser
  alias Zigler.Zig

  defmacro sigil_Z({:<<>>, meta, [zig_source]}, []) do

    # modify the zig code to include tests, if we're in test
    # environment.
    #  TODO: make this not call Mix.env in the module itself.
    #zig_code = if Mix.env() == :test do
    #  make_tests_funcs(zig_source)
    #else
    #  zig_source
    #end

    # parse the zig code here.
    tokens = Parser.tokenize(meta[:line], zig_source)

    code_spec = Code.to_spec(tokens)
    docs_spec = Code.to_docs(tokens)

    empty_functions = Enum.flat_map(code_spec, fn {func, {params, _type}} ->
      if docs_spec[func] do
        [{:@,
           [context: Elixir, import: Kernel],
           [{:doc, [context: Elixir], [docs_spec[func]]}]}]
      else
        []
      end
      ++
      [empty_function(func, params |> Zig.adjust_params |> Enum.count)]
    end)

    quote do
      @zig_code unquote(Code.to_iolist tokens)
      @zig_specs unquote(code_spec)
      unquote_splicing(empty_functions)
    end
  end

  defp empty_function(func, 0) do
    quote do
      def unquote(func)(), do: throw unquote("#{func} not defined")
    end
  end

  defp empty_function(func, arity) do
    {:def, [context: Elixir, import: Kernel],
    [
      {func, [context: Elixir], for _ <- 1..arity do {:_, [], Elixir} end},
      [
        do: {:throw, [context: Elixir, import: Kernel],
         ["#{func} not defined"]}
      ]
    ]}
  end

  def make_tests_funcs(code) do
    code
    |> String.split("\n")
    |> Enum.map(fn line ->
      cond do
        line =~ ~r/test(\s*)\".*\"(\s*){/ ->
          """
          @nif("__test0");
          fn __test0() void {
          """
        true -> line
      end
    end)
    |> Enum.join("\n")
  end

end
