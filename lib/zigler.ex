defmodule :zigler do
  @moduledoc """
  Parse transform module for using Zigler with erlang.

  For the canonical example, see:
  https://www.erlang.org/doc/man/erl_id_trans.html

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
    return "Hello, world!";
  }
  ")

  -zig_opts([{otp_app, my_app}]).
  ```

  This creates the `hello_world/0` function in your
  module which returns the "Hello, world!" binary.

  for options to be delivered in the `zig_opts` attribute, see the
  `Zig` module documentation.

  Note that the `...` for the `nifs` option is not representable in erlang AST.
  Instead, use the atom `auto`.

  > ### Note {: .warning }
  >
  > Erlang integration is highly experimental and the interface
  > may be changed in the future.
  """

  alias Zig.Compiler
  alias Zig.Options

  @doc """
  performs a parse transformation on the AST for an erlang module,
  converting public functions in the
  """
  def parse_transform(ast, _opts) do
    Application.ensure_all_started(:logger)
    Zig.Command.fetch!("0.10.1")

    file =
      Enum.find(ast, &match?({:attribute, _, :file, {_file, _}}, &1))
      |> elem(3)
      |> elem(0)

    opts =
      case Enum.find(ast, &match?({:attribute, _, :zig_opts, _}, &1))  do
        nil ->
          raise "No zig opts found"

        {:attribute, {line, _}, :zig_opts, opts} ->
          opts
          |> Keyword.merge(
            mod_file: file,
            mod_line: line,
            render: :render_erlang
          )
          |> Options.erlang_normalize!()
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
      |> Enum.sort(__MODULE__)

    if opts[:dump] do
      dump(ast)
    else
      ast
    end
  end

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

  defp ensure_libs do
    # we can't put this dependency into rebar.config because it has resolution issues
    # since jason decimal requirement is 1.0.0 or 2.0.0
    Enum.each(~w[jason zig_parser]a, fn lib ->
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
