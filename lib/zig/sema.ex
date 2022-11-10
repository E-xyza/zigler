defmodule Zig.Sema do
  alias Zig.Type
  alias Zig.Type.Struct

  require EEx
  alias Zig.Analyzer
  alias Zig.Assembler

  sema_zig_template = Path.join(__DIR__, "templates/sema.zig.eex")
  EEx.function_from_file(:defp, :file_for, sema_zig_template, [:opts])

  def analyze_file!(module, opts) do
    dir = Assembler.directory(module)
    sema_file = Path.join(dir, "sema.zig")

    File.write!(sema_file, file_for(opts))
    {result, 0} = System.cmd("zig", ["build", "sema"], cd: dir)

    %{"functions" => functions} = Jason.decode!(result)

    Enum.map(functions, fn function ->
      function
      |> Type.Function.from_json()
      |> Type.Function.scrub_structs(module)
      |> raise_if_private!(opts)
    end)
  end

  # verifies that none of the structs returned are private.
  defp raise_if_private!(function = %{name: name}, opts) do
    Enum.each(function.params, &raise_if_private!(&1, name, "accepts", opts))
    raise_if_private!(function.return, name, "returns", opts)
    function
  end

  defp raise_if_private!(%Struct{name: name}, function_name, verb, opts) do
    parsed = opts[:parsed]
    case Analyzer.info_for(parsed, name) do
      {:const, %{pub: false, position: const_position}, _} ->
        {:fn, fn_opts, _} = Analyzer.info_for(parsed, Atom.to_string(function_name))
        {fn_file, fn_line} = Analyzer.translate_location(parsed, opts[:file], fn_opts.position.line)
        {st_file, st_line} = Analyzer.translate_location(parsed, opts[:file], const_position.line)
        raise CompileError,
          file: fn_file,
          line: fn_line,
          description: "the function `#{function_name}` #{verb} the struct `#{name}` which is not public (defined at #{st_file}:#{st_line})"

      _ ->
        :ok
    end
  end

  defp raise_if_private!(_, _, _, _), do: :ok

end
