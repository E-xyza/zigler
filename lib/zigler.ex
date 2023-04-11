defmodule :zigler do

  alias Zig.Compiler

  def parse_transform(ast, _opts) do
    ensure_eex!()

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

    code_dir = case Keyword.fetch(opts, :src_dir) do
      {:ok, code_dir} ->
        code_dir
      _ -> raise "no src_dir found in zig_opts"
    end

    Compiler.compile(module, code_dir, Keyword.put(opts, :render, :render_erlang))

    ast
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
