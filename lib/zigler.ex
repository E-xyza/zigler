defmodule :zigler do
  alias Zig.Compiler
  alias Zig.Options

  def parse_transform(ast, _opts) do
    Application.ensure_all_started(:logger)
    Zig.Command.fetch("0.10.1")

    file =
      Enum.find(ast, &match?({:attribute, _, :file, {file, _}}, &1))
      |> elem(3)
      |> elem(0)

    opts =
      case Enum.find(ast, &match?({:attribute, _, :zig_opts, _}, &1)) do
        nil ->
          raise "No zig opts found"

        {:attribute, {line, _}, :zig_opts, opts} ->
          opts
          |> Keyword.merge(
            mod_file: file,
            mod_line: line,
            render: :render_erlang,
            ebin_dir: :priv
          )
          |> Options.normalize!()
      end

    otp_app =
      case Keyword.fetch(opts, :otp_app) do
        {:ok, otp_app} -> otp_app
        _ -> raise "No otp_app found in zig opts"
      end

    # utility functions:  Make sure certain apps are running
    # and make sure certain libraries are loaded.
    ensure_eex()
    ensure_libs()
    ensure_priv_dir(otp_app)

    {:attribute, _, :file, {file, _}} = Enum.find(ast, &match?({:attribute, _, :file, _}, &1))

    module_dir =
      file
      |> Path.dirname()
      |> Path.absname()

    module =
      case Enum.find(ast, &match?({:attribute, _, :module, _}, &1)) do
        nil -> raise "No module definition found"
        {:attribute, _, :module, module} -> module
      end

    code =
      ast
      |> Enum.flat_map(fn
        {:attribute, {line, _}, :zig_code, code} ->
          ["// ref #{file}:#{line}", code]

        _ ->
          []
      end)
      |> IO.iodata_to_binary()

    code_dir =
      case Keyword.fetch(opts, :code_dir) do
        {:ok, code_dir = "/" <> _} ->
          code_dir

        {:ok, code_dir} ->
          Path.join(module_dir, code_dir)

        _ ->
          module_dir
      end

    rendered_erlang = Compiler.compile(code, module, code_dir, opts)

    ast =
      ast
      |> Enum.reject(&match?({:attribute, _, :zig_code, _}, &1))
      |> append_attrib(:on_load, {:__init__, 0})
      |> append_attrib(:zig_code, code)
      |> Kernel.++(rendered_erlang)
      |> Enum.sort_by(&elem(&1, 0), __MODULE__)

    ast
    |> Enum.each(fn
      attribute when elem(attribute, 0) == :attribute -> :erl_pp.attribute(attribute)
      function when elem(function, 0) == :function -> :erl_pp.function(function)
      _ -> :ok
    end)

    if opts[:dump] do
      dump(ast)
    else
      ast
    end
  end

  defp append_attrib(ast, key, value), do: ast ++ [{:attribute, {1, 1}, key, value}]

  @order %{file: 0, attribute: 1, function: 2, error: 3, eof: 10}

  def compare(order1, order2) when is_integer(order1) and is_integer(order2) do
    case order1 do
      order1 when order1 < order2 -> :lt
      order1 when order1 > order2 -> :gt
      _ -> :eq
    end
  end

  def compare(type1, type2) when is_atom(type1) and is_atom(type2) do
    compare(Map.fetch!(@order, type1), Map.fetch!(@order, type2))
  end

  defp ensure_eex do
    # rebar_mix doesn't include the eex dependency out of the gate.  This function
    # retrieves the location of the eex code and adds it to the code path, ensuring
    # that the BEAM vm will have access to the eex code (at least, at compile time)
    :eex
    |> path_relative_to(:elixir)
    |> String.to_charlist()
    |> :code.add_path()
  end

  defp ensure_libs do
    # we can't put this dependency into rebar.config because it has resolution issues
    # since jason decimal requirement is 1.0.0 or 2.0.0
    Enum.each(~w(jason zig_parser)a, fn lib ->
      lib
      |> path_relative_to(:zigler)
      |> String.to_charlist()
      |> :code.add_path()
    end)
  end

  defp ensure_priv_dir(otp_app) do
    # makes sure the priv dir exists
    otp_app
    |> :code.lib_dir()
    |> Path.join("priv")
    |> File.mkdir_p!()
  end

  defp path_relative_to(target, start) do
    start
    |> :code.lib_dir()
    |> Path.join("../#{target}/ebin")
    |> Path.absname()
    |> to_string
  end

  defp dump(ast) do
    Enum.each(ast, fn
      tuple when elem(tuple, 0) == :attribute ->
        tuple
        |> :erl_pp.attribute()
        |> IO.puts()

      tuple when elem(tuple, 0) == :function ->
        tuple
        |> :erl_pp.function()
        |> IO.puts()

      _ ->
        :ok
    end)

    ast
  end
end
