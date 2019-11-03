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

    mod_name = __CALLER__.module |> Atom.to_string |> String.downcase
    nif_dir = Application.app_dir(opts[:app], "priv/nifs")
    zig_version = opts[:version] || @latest_zig_version

    File.mkdir_p!(nif_dir)

    quote do
      import Zigler

      @release_mode unquote(mode)

      @on_load :__load_nifs__
      @zigler_app unquote(opts[:app])
      @zig_version unquote(zig_version)

      def __load_nifs__ do
        nif_dir = unquote(nif_dir)
        mod_name = unquote(mod_name)

        nif_dir
        |> Path.join(mod_name)
        |> String.to_charlist()
        |> :erlang.load_nif(0)
      end

      Module.register_attribute(__MODULE__, :zig_specs, accumulate: true)
      Module.register_attribute(__MODULE__, :zig_code, accumulate: true)

      @before_compile Zigler.Compiler
    end
  end

  defmacro sigil_Z({:<<>>, meta, [zig_code]}, []) do
    file = __CALLER__.file
    line = meta[:line]

    # perform code analysis
    code = Zigler.Code.from_string(zig_code, file, line)

    code_spec = Enum.map(code.nifs, &{&1.name, {&1.params, &1.retval}})

    Enum.map(code.nifs, &(&1.name))

    empty_functions = Enum.flat_map(code.nifs, fn nif ->
      if nif.doc do
        [{:@,
           [context: Elixir, import: Kernel],
           [{:doc, [context: Elixir], [nif.doc]}]}]
      else
        []
      end
      ++
      [empty_function(nif.name, nif.arity)]
    end)

    quote do
      @zig_code unquote(code.code)
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

end
