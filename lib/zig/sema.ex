defmodule Zig.Sema do
  alias Zig.Type

  require EEx
  alias Zig.Assembler
  alias Zig.Parser

  sema_zig_template = Path.join(__DIR__, "templates/sema.zig.eex")
  EEx.function_from_file(:defp, :file_for, sema_zig_template, [:opts])

  def analyze_file!(module, opts) do
    dir = Assembler.directory(module)
    sema_file = Path.join(dir, "sema.zig")

    File.write!(sema_file, file_for(opts))
    {result, 0} = System.cmd("zig", ["build", "sema"], cd: dir)

    %{"functions" => functions, "privates" => privates} = Jason.decode!(result)

    functions
    |> Enum.map(&Type.Function.from_json/1)
    |> Enum.map(&check_privates(&1, privates, opts))
  end

  # verifies that none of the structs involved are private.
  defp check_privates(function = %{return: %{name: name}}, privates, opts) do
    if Enum.any?(privates, &(name == "nif." <> &1)) do
      parsed_file =
        opts
        |> Keyword.fetch!(:file)
        |> Parser.parse

      raise "foo"

      "nif." <> struct_name = name

      #raise CompileError,
      #  file: fn_file,
      #  line: fn_line,
      #  description:
      #    "the function #{function.name} returns the struct #{struct_name} which is not public"
    end

    function
  end

  defp check_privates(function, _, _), do: function
end
