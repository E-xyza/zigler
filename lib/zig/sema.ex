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
  end
end
