defmodule Zigler do

  defmacro __using__(opts) do

    unless opts[:app] do
      raise ArgumentError, "you must provide the application"
    end

    # make sure that we're in the correct operating system.
    unless {:unix, :linux} == :os.type() do
      raise "non-linux systems not currently supported."
    end

    mod_name = __CALLER__.module |> Atom.to_string |> String.downcase
    nif_dir = Application.app_dir(opts[:app], "priv/nifs")

    File.mkdir_p!(nif_dir)

    quote do
      import Zigler

      @on_load :__load_nifs__
      @zigler_app unquote(opts[:app])

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

  alias Zigler.Zig

  defmacro sigil_Z({:<<>>, _meta, [zig_code]}, []) do

    # first do an analysis on the zig code.
    code_spec = Zig.code_spec(zig_code)

    empty_functions = Enum.map(code_spec, fn {func, {params, _type}} ->
      empty_function(func, params |> Zig.adjust_params |> Enum.count)
    end)

    quote do
      @zig_code unquote(Zig.strip_nif(zig_code))
      @zig_specs unquote(code_spec)
      unquote_splicing(empty_functions)
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
