defmodule Zig.Sema do
  alias Zig.Type

  require EEx
  alias Zig.Assembler
  alias Zig.Command

  sema_zig_template = Path.join(__DIR__, "templates/sema.zig.eex")
  EEx.function_from_file(:defp, :file_for, sema_zig_template, [:assigns])

  @spec analyze_file!(module :: module, opts :: keyword) :: [Function.t()]
  def analyze_file!(module, opts) do
    dir = Assembler.directory(module)
    sema_file = Path.join(dir, "sema.zig")

    File.write!(sema_file, file_for(opts))
    Command.fmt(sema_file)
    result = Command.build_sema(dir)

    function_json =
      result
      |> Jason.decode!()
      |> Map.fetch!("functions")

    nif_opts =
      case Keyword.fetch!(opts, :nifs) do
        {:auto, specified} ->
          specified_functions = Keyword.keys(specified)

          Enum.flat_map(function_json, fn function ->
            name = String.to_atom(function["name"])
            List.wrap(if name not in specified_functions, do: {name, []})
          end) ++ specified

        keyword when is_list(keyword) ->
          keyword
      end

    Enum.map(function_json, fn function ->
      name = String.to_atom(function["name"])

      fn_opts =
        nif_opts
        |> Keyword.fetch!(name)
        |> normalize

      Type.Function.from_json(function, module, name, fn_opts)
    end)
  end

  def normalize(opts) do
    opts
    |> Keyword.put_new(:args, %{})
    |> Keyword.put_new(:return, type: :default)
    |> Keyword.update!(:return, &normalize_return/1)
  end

  def normalize_return(return) do
    return
    |> List.wrap()
    |> Enum.map(fn
      :binary -> {:type, :binary}
      :charlist -> {:type, :charlist}
      integer when is_integer(integer) -> {:arg, integer}
      other -> other
    end)
  end
end
