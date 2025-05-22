defmodule :zigler do
  @moduledoc """
  Parse transform module for using Zigler with erlang.

  ## Prerequisites

  In order to use Zigler in an erlang project, you must have the Elixir
  runtime.   You may do this any way you wish, but Zigler recommends
  rebar_mix:

  https://github.com/Supersonido/rebar_mix

  There are instructions on how to make sure Elixir is available at
  compile time for your erlang project.

  ## Building a Zig Module

  General documentation on parse transforms is very light.  To use zigler as
  a parse transform:

  ```erlang
  -module(my_erlang_module).
  -compile({parse_transform, zigler}).
  -export([...]).

  -zig_code("
  pub fn hello_world() [] const u8 {
    return \\"Hello, world!\\";
  }
  ")

  -zig_opts([{otp_app, my_app}]).
  ```

  This creates the `hello_world/0` function in your
  module which returns the "Hello, world!" binary.

  for options to be delivered in the `zig_opts` attribute, see the
  `Zig` module documentation.

  Note that the `...` for the `nifs` option is not representable in erlang AST.
  Instead, use the tuple `{auto, [<nifs options...>]}`

  > ### Note {: .warning }
  >
  > Erlang integration is experimental and the interface
  > may be changed in the future.
  """

  alias Zig.Compiler
  alias Zig.EasyC

  @doc """
  performs a parse transformation on the AST for an erlang module,
  converting public functions in the zig code into erlang functions.
  """
  def parse_transform(ast, _opts) do
    Application.ensure_all_started(:logger)
    {_, module} = get_attribute(ast, :module)
    {_, {file, _}} = get_attribute(ast, :file)
    {line, opts} = get_attribute(ast, :zig_opts) || raise "No zig opts found"

    opts =
      opts
      |> Keyword.put(:language, :erlang)
      |> normalize_erlang_nif_spec()
      |> Zig.Module.new(%{file: file, line: line, module: module})

    code_dir =
      case {opts.dir, file} do
        {nil, file} -> Path.dirname(file)
        {dir, _} -> dir
      end

    # utility functions:  Make sure certain apps are running
    # and make sure certain libraries are loaded.
    ensure_eex()
    ensure_libs()
    ensure_priv_dir(opts.otp_app)

    code =
      if opts.easy_c do
        EasyC.build_from(opts)
      else
        ast
        |> Enum.flat_map(fn
          {:attribute, {line, _}, :zig_code, code} ->
            ["// ref #{file}:#{line}", code]

          _ ->
            []
        end)
        |> IO.iodata_to_binary()
      end

    rendered = Compiler.compile(code, code_dir, opts)

    ast =
      ast
      |> Enum.reject(&match?({:attribute, _, :zig_code, _}, &1))
      |> append_attrib(:on_load, {:__init__, 0})
      |> append_attrib(:zig_code, code)
      |> Kernel.++(rendered)
      |> Enum.sort(__MODULE__)

    if opts.dump do
      dump(ast)
    else
      ast
    end
  rescue
    e ->
      require Logger

      Logger.error(
        "Error compiling Zigler: #{Exception.message(e)}\n\n#{Exception.format_stacktrace(__STACKTRACE__)}"
      )
  end

  @spec get_attribute(list, atom) :: {non_neg_integer, term} | nil
  defp get_attribute(ast, attribute) do
    Enum.find_value(ast, fn
      {:attribute, {line, _}, ^attribute, payload} -> {line, payload}
      _ -> nil
    end)
  end

  defp normalize_erlang_nif_spec(spec) do
    spec
    |> Keyword.update(:c, [], &update_c/1)
    |> Keyword.put_new(:nifs, {:auto, []})
    |> update_if(:easy_c, &stringify/1)
  end

  defp update_c(c_opts), do: update_if(c_opts, :easy_c, &stringify/1)

  defp update_if(keyword, key, fun) do
    Enum.map(keyword, fn
      {^key, value} -> {key, fun.(value)}
      other -> other
    end)
  end

  defp stringify(charlist) when is_list(charlist), do: IO.iodata_to_binary(charlist)
  defp stringify({:priv, payload}), do: {:priv, stringify(payload)}
  defp stringify({:system, payload}), do: {:system, stringify(payload)}
  defp stringify(other), do: other

  defp append_attrib(ast, key, value), do: ast ++ [{:attribute, {1, 1}, key, value}]

  @order %{file: 0, attribute: 1, function: 2, error: 3, eof: 10}

  @doc false
  # This is a comparison function for sorting the AST.  By implementing
  # the compare/2 informal behaviour, we are able to sort the contents
  # of erlang ast
  def compare(ast1, ast2) do
    case {Map.fetch!(@order, elem(ast1, 0)), Map.fetch!(@order, elem(ast2, 0))} do
      {order1, order2} when order1 < order2 -> :lt
      {order1, order2} when order1 > order2 -> :gt
      _ -> :eq
    end
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

  case Code.ensure_loaded(:json) do
    {:module, :json} ->
      @libs [:zig_parser]

    _ ->
      @libs ~w[jason zig_parser]a
  end

  defp ensure_libs do
    # we can't put this dependency into rebar.config because it has resolution issues
    # since jason decimal requirement is 1.0.0 or 2.0.0
    Enum.each(@libs, fn lib ->
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
