defmodule Zig.Sema do
  require EEx
  alias Zig.Assembler
  alias Zig.Command
  alias Zig.Type.Function

  sema_zig_template = Path.join(__DIR__, "templates/sema.zig.eex")
  EEx.function_from_file(:defp, :file_for, sema_zig_template, [:assigns])

  @spec analyze_file!(module :: module, opts :: keyword) :: {%{atom() => Function.t()}, keyword}
  def analyze_file!(module, opts) do
    dir = Assembler.directory(module)
    sema_file = Path.join(dir, "sema.zig")

    File.write!(sema_file, file_for(opts))
    Command.fmt(sema_file)
    result = Command.build_sema(dir)

    functions_json =
      result
      |> Jason.decode!()
      |> Map.fetch!("functions")

    case Keyword.fetch!(opts, :nifs) do
      {:auto, specified} ->
        default_options = Keyword.fetch!(opts, :default_options)

        # make sure all of the specified functions are in the nif list.
        {mapped_functions, new_fn_opts} =
          functions_json
          |> Enum.map(fn function_json = %{"name" => name} ->
            atom_name = String.to_atom(name)

            if function_opts = Keyword.get(specified, atom_name) do
              {{atom_name, function_type(function_json, module, function_opts)},
               {atom_name, function_opts}}
            else
              {{atom_name, function_type(function_json, module, default_options)},
               {atom_name, default_options}}
            end
          end)
          |> Enum.unzip()

        {Map.new(mapped_functions), new_fn_opts}

      keyword when is_list(keyword) ->
        {Map.new(keyword, fn {name, opts} ->
           find(name, functions_json, module, opts)
         end), keyword}
    end
  end

  defp find(name, functions_json, module, opts) do
    str_name = Atom.to_string(name)
    str_alias = if alias_ = Keyword.get(opts, :alias), do: Atom.to_string(alias_)

    cond do
      function_json = Enum.find(functions_json, &(&1["name"] == str_name)) ->
        {name, function_type(function_json, module, opts)}

      function_json = Enum.find(functions_json, &(&1["name"] == str_alias)) ->
        {name, function_type(function_json, module, opts)}

      true ->
        raise "function #{name} was specified in the nif list, but was not found in the zig file."
    end
  end

  defp function_type(function_json, module, opts) do
    found_function = Function.from_json(function_json, module)

    case opts[:raw] do
      nil ->
        found_function

      integer when is_integer(integer) ->
        %{found_function | params: List.duplicate(:term, integer), arity: integer}

      {:c, integer} when is_integer(integer) ->
        %{found_function | params: List.duplicate(:erl_nif_term, integer), arity: integer}
    end
  end
end
