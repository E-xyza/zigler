defmodule Zig.Sema do
  @moduledoc false
  require EEx
  alias Zig.Manifest
  alias Zig.Module
  alias Zig.Type
  alias Zig.Type.Function
  alias Zig.Type.Manypointer
  alias Zig.Type.Struct

  @enforce_keys [:functions, :types, :decls]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          functions: [Function.t()],
          types: keyword(Type.t()),
          decls: keyword(Type.t())
        }

  # PHASE 1:  SEMA EXECUTION

  @spec run_sema!(Module.t()) :: Module.t()
  # performs the first stage of semantic analysis:
  # actually executing the zig command to obtain the semantic analysis of the
  # desired file.
  def run_sema!(module) do
    module.nif_code_path
    |> Zig.Command.run_sema!(module)
    |> Jason.decode!()
    |> tap(&maybe_dump(&1, module))
    |> validate_callbacks(module)
    |> reject_ignored(module)
    |> integrate_sema(module)
    |> then(&Map.replace!(module, :sema, &1))
  rescue
    e in Zig.CompileError ->
      reraise Zig.CompileError.resolve(e, module), __STACKTRACE__
  end

  defp validate_callbacks(sema, _module) do
    IO.warn("not implemented yet")
    sema
  end

  # removes "ignored" and "callback" functions from the semantic analysis.
  defp reject_ignored(json, module) do
    ignored = Enum.map(module.ignore, &"#{&1}") ++ Enum.map(module.callbacks, &"#{elem(&1, 1)}")

    Map.update!(json, "functions", fn
      functions ->
        Enum.reject(functions, &(&1["name"] in ignored))
    end)
  end

  defp integrate_sema(%{"functions" => functions, "types" => types, "decls" => decls}, module) do
    %__MODULE__{
      functions: Enum.map(functions, &Function.from_json(&1, module)),
      types: Enum.map(types, &type_from_json(&1, module)),
      decls: Enum.map(decls, &const_from_json/1)
    }
  end

  defp type_from_json(%{"name" => name, "type" => type}, module) do
    %{name: String.to_atom(name), type: Type.from_json(type, module)}
  end

  defp const_from_json(%{"name" => name, "type" => type}) do
    %{name: String.to_atom(name), type: String.to_atom(type)}
  end

  defp maybe_dump(sema_json, module) do
    if module.dump_sema do
      sema_json_pretty = Jason.encode!(sema_json, pretty: true)
      IO.puts([IO.ANSI.yellow(), sema_json_pretty, IO.ANSI.reset()])
    end
  end

  @spec analyze_file!(Module.t()) :: Module.t()
  # updates the per-function options to include the semantically understood type
  # information.  Also strips "auto" from the nif information to provide a finalized
  # keyword list of functions with their options.
  def analyze_file!(%{sema: %{functions: functions, types: types}} = module) do
    # Enum.each(functions, &validate_usable!(&1, types, module))

    # `nifs` option could either be {:auto, keyword} which means that the full
    # list of functions should be derived from the semantic analysis, determining
    # which functions have `pub` declaration, with certain functions optionally
    # having their specification overloaded.
    #
    # it could also be just a list of functions with their specifications, in
    # which case those are the *only* functions that will be included.

    nifs = case module.nifs do
      {:auto, specified_fns} ->
        Enum.map(functions, fn function ->
          # TODO: apply options from specified functions, if it matches.

          function
        end)
      #    default_options = Keyword.fetch!(module, :default_options)
      #
      #    # make sure all of the specified functions are indeed found by sema.
      #    Enum.each(specified_fns, fn
      #      {specified_fn, _} ->
      #        unless Enum.any?(functions, &(&1.name == specified_fn)) do
      #          raise CompileError,
      #            description:
      #              "function #{specified_fn} not found in semantic analysis of functions."
      #        end
      #    end)
      #
      #    Enum.map(
      #      functions,
      #      fn function = %{name: name} ->
      #        function_opts = Keyword.get(specified_fns, name, default_options)
      #
      #        adjusted_function = adjust_raw(function, function_opts)
      #
      #        {name, Keyword.put(function_opts, :type, adjusted_function)}
      #      end
      #    )
      #
      selected_fns when is_list(selected_fns) ->
        Enum.map(selected_fns, fn {name, function_opts} ->
          if function = Enum.find(functions, &(&1.name == name)) do
            name
            |> Nif.new(function_opts)
            |> apply_from_sema(function)
          else
            raise CompileError,
              description: "function #{name} not found in semantic analysis of functions.",
              file: module.file
          end
        end)

        # TODO: warn when a function
    end

    %{module | nifs: nifs}
  end

  defp apply_from_sema(nif, sema) do
    nif
  end

  #  defp adjust_raw(function, opts) do
  #    case {function, opts[:raw]} do
  #      {_, nil} ->
  #        function
  #
  #      # use this to identify that the function is a raw call.  `child` could be
  #      # either :term or :erl_nif_term.
  #      {%{params: [:env, _, %Manypointer{child: child}]}, integer} when is_integer(integer) ->
  #        %{function | params: List.duplicate(child, integer), arity: integer}
  #
  #      {_, {:c, integer}} when is_integer(integer) ->
  #        %{function | params: List.duplicate(:erl_nif_term, integer), arity: integer}
  #    end
  #  end
  #
  #
  

  #
  #  defp validate_usable!(function, types, opts) do
  #    with :ok <- validate_args(function, types),
  #         :ok <- validate_return(function.return),
  #         :ok <- validate_struct_pub(function.return, types, "return", function.name) do
  #      :ok
  #    else
  #      {:error, msg} ->
  #        file_path =
  #          opts
  #          |> Keyword.fetch!(:mod_file)
  #          |> Path.relative_to_cwd()
  #
  #        %{location: {raw_line, _}} =
  #          opts
  #          |> Keyword.fetch!(:parsed)
  #          |> find_function(function.name)
  #
  #        {file, line} =
  #          opts
  #          |> Keyword.fetch!(:manifest)
  #          |> Manifest.resolve(file_path, raw_line)
  #
  #        raise CompileError,
  #          description: msg,
  #          file: file,
  #          line: line
  #    end
  #  end
  #
  #  defp find_function(%{code: code}, function) do
  #    Enum.find(code, fn
  #      %Zig.Parser.Function{name: ^function} -> true
  #      %Zig.Parser.Const{name: ^function} -> true
  #      _ -> false
  #    end)
  #  end
  #
  #  defp validate_args(function, types) do
  #    function.params
  #    |> Enum.with_index(1)
  #    |> Enum.reduce_while(:ok, fn
  #      {type, index}, :ok ->
  #        case validate_struct_pub(type, types, "argument #{index}", function.name) do
  #          :ok ->
  #            {:cont, :ok}
  #
  #          error = {:error, _msg} ->
  #            {:halt, error}
  #        end
  #    end)
  #  end
  #
  #  # explicitly, all return values that are permitted
  #  defp validate_return(type) do
  #    if Type.return_allowed?(type) do
  #      :ok
  #    else
  #      {:error, "functions returning #{type} cannot be nifs"}
  #    end
  #  end
  #
  #  defp validate_struct_pub(%Struct{name: name}, types, location, function) do
  #    Enum.reduce_while(
  #      types,
  #      {:error, "struct `#{name}` (#{location} of function `#{function}`) is not a `pub` struct"},
  #      fn
  #        %{name: type_name}, error = {:error, _} ->
  #          if Atom.to_string(type_name) == name do
  #            {:halt, :ok}
  #          else
  #            {:cont, error}
  #          end
  #      end
  #    )
  #  end
  #
  #  defp validate_struct_pub(_, _, _, _), do: :ok
end
