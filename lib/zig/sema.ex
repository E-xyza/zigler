defmodule Zig.Sema do
  alias Zig.Type

  require EEx
  alias Zig.Assembler

  sema_zig_template = Path.join(__DIR__, "templates/sema.zig.eex")
  EEx.function_from_file(:defp, :file_for, sema_zig_template, [:opts])

  def analyze_file!(module, opts) do
    dir = Assembler.directory(module)
    sema_file = Path.join(dir, "sema.zig")

    File.write!(sema_file, file_for(opts))
    {result, 0} = System.cmd("zig", ["build", "sema"], cd: dir)

    result
    |> Jason.decode!()
    |> Enum.map(&Type.Function.from_json/1)

    # result
    # |> String.trim()
    # |> String.split("\n")
    # |> Enum.map(fn line ->
    #  [name, return | params] = String.split(line)
    #
    #  return = Type.parse(return)
    #
    #  params =
    #    case Enum.map(params, &Type.parse/1) do
    #      [Env | params] -> params
    #      params -> params
    #    end
    #
    #  # for now, we will need to take out beam.env's.
    #  arity = length(params)
    #
    #  %Zig.Type.Function{name: String.to_atom(name), arity: arity, params: params, return: return}
    # end)
  end
end
