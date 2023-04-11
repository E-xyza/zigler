defmodule :zigler do

  alias Zig.Compiler

  def parse_transform(ast, _opts) do
    Application.ensure_all_started(:logger)
    Zig.Command.fetch("0.10.0")

    ensure_eex!()

    {:attribute, _, :file, {file, _}} = Enum.find(ast, &match?({:attribute, _, :file, _}, &1))
    module_dir = Path.dirname(file)

    exports = Enum.reduce(ast, [], fn
      {:attribute, _, :export, exports}, acc -> exports ++ acc
      _, acc -> acc
    end)

    module = case Enum.find(ast, &match?({:attribute, _, :module, _}, &1)) do
      nil -> raise "No module definition found"
      {:attribute, _, :module, module} -> module
    end

    {code, pos} =
      case Enum.find(ast, &match?({:attribute, _, :zig, _}, &1)) do
        nil -> raise "No zig code found"
        {:attribute, pos, :zig, code} -> {code, pos}
      end

    opts =
      case Enum.find(ast, &match?({:attribute, _, :zig_opts, _}, &1)) do
        nil -> raise "No zig opts found"
        {:attribute, _, :zig_opts, opts} -> opts
      end

    code_dir = case Keyword.fetch(opts, :code_dir) do
      {:ok, code_dir = "/" <> _} ->
        code_dir
      {:ok, code_dir} ->
        Path.join(module_dir, code_dir)
      _ ->
        module_dir
    end

    opts = opts
    |> Zig.normalize!
    |> Keyword.put(:render, :render_erlang)

    Compiler.compile(code, module, code_dir, opts)

    ast
    |> Enum.reject(&match?({:attribute, _, :exports, _}, &1))
    |> insert(:exports, exports)
  end

  defp insert(ast, key, value) do
    [eof = {:eof, {line, _}} | rest] = Enum.reverse(ast)
    Enum.reverse([eof, {:attribute, {line, 1}, key, value} | rest])
  end

  defp ensure_eex! do
    # rebar_mix doesn't include the eex dependency out of the gate.  This function
    # retrieves the location of the eex code and adds it to the code path, ensuring
    # that the BEAM vm will have access to the eex code (at least, at compile time)

    elixir = :elixir
    |> :code.lib_dir()
    |> Path.join("../eex/ebin")
    |> Path.absname
    |> to_string
    |> String.to_charlist
    |> :code.add_path
  end
end
