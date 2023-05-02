defmodule Zig.Sema do
  alias Zig.Type

  require EEx
  alias Zig.Assembler
  alias Zig.Command

  sema_zig_template = Path.join(__DIR__, "templates/sema.zig.eex")
  EEx.function_from_file(:defp, :file_for, sema_zig_template, [:assigns])

  def analyze_file!(module, opts) do
    dir = Assembler.directory(module)
    sema_file = Path.join(dir, "sema.zig")

    File.write!(sema_file, file_for(opts))
    Command.fmt(sema_file)
    result = Command.build_sema(dir)

    functions =
      result
      |> Jason.decode!()
      |> Map.fetch!("functions")

    nif_opts =
      case Keyword.fetch!(opts, :nifs) do
        {:all, specified} ->
          specified_functions = Keyword.keys(specified)

          Enum.flat_map(functions, fn function ->
            name = String.to_atom(function["name"])
            List.wrap(if name not in specified_functions, do: {name, []})
          end) ++ specified

        keyword when is_list(keyword) ->
          keyword
      end

    Enum.map(functions, fn function ->
      name = String.to_atom(function["name"])
      fn_opts = Keyword.fetch!(nif_opts, name)

      Type.Function.from_json(function, module, name, fn_opts)
    end)
  end
end
