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

    nif_opts =
      case Keyword.fetch!(opts, :nifs) do
        :all -> Enum.map(functions, &{String.to_atom(&1["name"]), []})
        keyword when is_list(keyword) -> keyword
      end

    Enum.map(functions, fn function ->
      name = String.to_atom(function["name"])
      fn_opts = Keyword.fetch!(nif_opts, name)

      Type.Function.from_json(function, module, name, fn_opts)
    end)
  end
end
