defmodule Zig.Sema do
  require EEx
  alias Zig.Assembler
  alias Zig.Command
  alias Zig.Manifest
  alias Zig.Type
  alias Zig.Type.Function

  @spec analyze_file!(module :: module, Manifest.t(), opts :: keyword) :: keyword
  # updates the per-function options to include the semantically understood type
  # information.  Also strips "auto" from the nif information to provide a finalized
  # keyword list of functions with their options.
  def analyze_file!(module, manifest, opts) do
    # dir = Assembler.directory(module)
    file = Keyword.fetch!(opts, :file)

    functions =
      case run_sema(file, module, opts) do
        {:ok, sema} -> sema.functions
      end

    # `nifs` option could either be {:auto, keyword} which means that the full
    # list of functions should be derived from the semantic analysis, determining
    # which functions have `pub` declaration, with certain functions optionally
    # having their specification overloaded.
    #
    # it could also be just a list of functions with their specifications, in
    # which case those are the *only* functions that will be included.

    case Keyword.fetch!(opts, :nifs) do
      {:auto, specified_fns} ->
        default_options = Keyword.fetch!(opts, :default_options)

        # make sure all of the specified functions are indeed found by sema.
        Enum.each(specified_fns, fn
          {specified_fn, _} ->
            unless Enum.any?(functions, &(&1.name == specified_fn)),
              do:
                raise(
                  CompileError,
                  "function #{specified_fn} not found in semantic analysis of functions."
                )
        end)

        Enum.map(
          functions,
          fn function = %{name: name} ->
            function_opts = Keyword.get(specified_fns, name, default_options)

            adjusted_function = adjust_raw(function, function_opts)

            {name, Keyword.put(function_opts, :type, adjusted_function)}
          end
        )

      selected_fns when is_list(selected_fns) ->
        Enum.map(selected_fns, fn
          {selected_fn, function_opts} ->
            function = Enum.find(functions, &(&1.name == selected_fn))

            unless function,
              do:
                raise(
                  CompileError,
                  "function #{selected_fn} not found in semantic analysis of functions."
                )

            adjusted_function = adjust_raw(function, function_opts)

            {selected_fn, Keyword.put(function_opts, :type, adjusted_function)}
        end)
    end
  rescue
    e in Zig.CompileError ->
      reraise Zig.CompileError.to_error(e, manifest), __STACKTRACE__
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

  def run_sema(file, module \\ nil, opts \\ []) do
    # TODO: integrate error handling here, and make this a common nexus for
    # file compilation
    with {:ok, sema_str} <- Zig.Command.run_sema(file, opts),
         {:ok, sema_json} <- Jason.decode(sema_str) do

      if opts[:dump_sema] do
        sema_json_pretty = Jason.encode!(sema_json, pretty: true)
        IO.puts([IO.ANSI.yellow(), sema_json_pretty, IO.ANSI.reset()])
      end

      # filter out any functions present in "ignore" clause.
      sema_json
      |> Map.update!("functions", &filter_ignores(&1, opts))
      |> module_to_types(module)
    end
  end

  defp filter_ignores(functions, opts) do
    ignores =
      opts
      |> Keyword.get(:ignore, [])
      |> List.wrap()
      |> Enum.map(&to_string/1)

    Enum.reject(functions, &(&1["name"] in ignores))
  end

  defp module_to_types(%{"functions" => functions, "types" => types, "decls" => decls}, module) do
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
