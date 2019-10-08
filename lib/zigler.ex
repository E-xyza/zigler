defmodule Zigler do

  @latest_zig_version Application.get_env(:zigler, :latest_zig_version)

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
    zig_version = opts[:version] || @latest_zig_version

    File.mkdir_p!(nif_dir)

    quote do
      import Zigler

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

  alias Zigler.Zig

  defmacro sigil_Z({:<<>>, _meta, [zig_code]}, []) do

    # first do an analysis on the zig code.
    {doc_spec, code_spec} = zig_code
    |> Zig.code_spec
    |> Enum.split_with(fn
      {:doc, _} -> true
      _ -> false
    end)

    doc_kw = Enum.map(doc_spec, fn {_, kw} -> kw end)

    empty_functions = Enum.flat_map(code_spec, fn {func, {params, _type}} ->
      if doc_kw[func] do
        [{:@,
           [context: Elixir, import: Kernel],
           [{:doc, [context: Elixir], [doc_kw[func]]}]}]
      else
        []
      end
      ++
      [empty_function(func, params |> Zig.adjust_params |> Enum.count)]
    end)

    quote do
      @zig_code unquote(Zig.strip_nif(zig_code))
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
