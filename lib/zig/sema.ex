defmodule Zig.Sema do
  require EEx
  alias Zig.Assembler
  alias Zig.Command
  alias Zig.Manifest
  alias Zig.Type
  alias Zig.Type.Function

  @spec analyze_file!(module :: module, Manifest.t(), opts :: keyword) ::
          {%{atom() => Function.t()}, keyword}
  def analyze_file!(module, manifest, opts) do
    # dir = Assembler.directory(module)
    file = Keyword.fetch!(opts, :file)

    functions =
      case run_sema(file, module, opts) |> dbg(limit: 25) do
        {:ok, sema} ->
          filter_ignores(sema.functions, opts)
      end

    case Keyword.fetch!(opts, :nifs) |> dbg(limit: 25) do
      {:auto, specified} ->
        default_options = Keyword.fetch!(opts, :default_options)

        # make sure all of the specified functions are in the nif list.
        Enum.reduce(
          functions,
          {Map.new(), []},
          # TODO: make this its own function
          fn function = %{name: name}, {new_types, new_opts} ->
            function_opts = Keyword.get(opts, name, default_options)
            # TODO: verify that this is/isn't necessary.
            new_function = adjust_raw(function, function_opts)

            {Map.put(new_types, name, function), Keyword.put(new_opts, name, function_opts)}
          end
        )

      fn_opts when is_list(fn_opts) ->
        raise "unimplemented"

        #{Map.new(keyword, fn {name, opts} ->
        #   find(name, functions_json, module, opts)
        # end), keyword}
    end

#    raise "foo"
  rescue
    e in Zig.CompileError ->
      reraise Zig.CompileError.to_error(e, manifest), __STACKTRACE__
  end

  defp find(name, functions_json, module, opts) do
    #str_name = Atom.to_string(name)
    #str_alias = if alias_ = Keyword.get(opts, :alias), do: Atom.to_string(alias_)
#
    #cond do
    #  function_json = Enum.find(functions_json, &(&1["name"] == str_name)) ->
    #    {name, function_type(function_json, module, opts)}
#
    #  function_json = Enum.find(functions_json, &(&1["name"] == str_alias)) ->
    #    {name, function_type(function_json, module, opts)}
#
    #  true ->
    #    raise "function #{name} was specified in the nif list, but was not found in the zig file."
    #end
  end

  defp adjust_raw(function, opts) do
    case opts[:raw] do
      nil ->
        function

      integer when is_integer(integer) ->
        %{function | params: List.duplicate(:term, integer), arity: integer}

      {:c, integer} when is_integer(integer) ->
        %{function | params: List.duplicate(:erl_nif_term, integer), arity: integer}
    end
  end

  defp filter_ignores(functions, opts) do
    ignores =
      opts
      |> Keyword.get(:ignore, [])
      |> List.wrap()
      |> Enum.map(&to_string/1)

    Enum.reject(functions, &(&1.name in ignores))
  end

  def run_sema(file, module \\ nil, opts \\ []) do
    # TODO: integrate error handling here, and make this a common nexus for
    # file compilation
    with {:ok, sema_str} <- Zig.Command.run_sema(file),
         {:ok, sema_json} <- Jason.decode(sema_str) do
      if opts[:dump_sema] do
        sema_json_pretty = Jason.encode!(sema_json, pretty: true)
        IO.puts([IO.ANSI.yellow(), sema_json_pretty, IO.ANSI.reset()])
      end

      module_to_types(sema_json, module)
    end
  end

  def module_to_types(%{"functions" => functions, "types" => types, "decls" => decls}, module) do
    {:ok,
     %{
       functions: Enum.map(functions, &Function.from_json(&1, module)),
       types: Enum.map(types, &type_from_json(&1, module)),
       decls: Enum.map(decls, &const_from_json/1)
     }}
  end

  defp type_from_json(%{"name" => name, "type" => type}, module) do
    %{name: String.to_atom(name), type: Type.from_json(type, module)}
  end

  defp const_from_json(%{"name" => name, "type" => type}) do
    %{name: String.to_atom(name), type: String.to_atom(type)}
  end
end
