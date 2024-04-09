defmodule Zig.Sema do
  require EEx
  alias Zig.Manifest
  alias Zig.Type
  alias Zig.Type.Function
  alias Zig.Type.Manypointer
  alias Zig.Type.Struct

  @spec analyze_file!(module :: map, opts :: keyword) :: keyword
  # updates the per-function options to include the semantically understood type
  # information.  Also strips "auto" from the nif information to provide a finalized
  # keyword list of functions with their options.
  def analyze_file!(%{functions: functions, types: types}, opts) do
    Enum.each(functions, &validate_usable!(&1, types, opts))

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
            unless Enum.any?(functions, &(&1.name == specified_fn)) do
              raise CompileError,
                description:
                  "function #{specified_fn} not found in semantic analysis of functions."
            end
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

            unless function do
              raise CompileError,
                description:
                  "function #{selected_fn} not found in semantic analysis of functions."
            end

            adjusted_function = adjust_raw(function, function_opts)

            {selected_fn, Keyword.put(function_opts, :type, adjusted_function)}
        end)
    end
  end

  defp adjust_raw(function, opts) do
    case {function, opts[:raw]} do
      {_, nil} ->
        function

      # use this to identify that the function is a raw call.  `child` could be
      # either :term or :erl_nif_term.
      {%{params: [:env, _, %Manypointer{child: child}]}, integer} when is_integer(integer) ->
        %{function | params: List.duplicate(child, integer), arity: integer}

      {_, {:c, integer}} when is_integer(integer) ->
        %{function | params: List.duplicate(:erl_nif_term, integer), arity: integer}
    end
  end

  def run_sema!(opts) do
    opts.nif_code_path
    |> Zig.Command.run_sema!(opts)
    |> Jason.decode!()
    |> tap(&maybe_dump(&1, opts))
    |> filter_ignores(opts)
    |> module_to_types(opts)
  rescue
    e in Zig.CompileError ->
      reraise Zig.CompileError.to_error(e, opts), __STACKTRACE__
  end

  defp maybe_dump(sema_json, opts) do
    if opts.dump_sema do
      sema_json_pretty = Jason.encode!(sema_json, pretty: true)
      IO.puts([IO.ANSI.yellow(), sema_json_pretty, IO.ANSI.reset()])
    end
  end

  defp filter_ignores(json, opts) do
    ignores = Enum.map(opts.ignore, &"#{&1}")
    Map.update!(json, "functions", fn 
      functions ->
        Enum.reject(functions, &(&1["name"] in ignores))
    end)
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

  defp validate_usable!(function, types, opts) do
    with :ok <- validate_args(function, types),
         :ok <- validate_return(function.return),
         :ok <- validate_struct_pub(function.return, types, "return", function.name) do
      :ok
    else
      {:error, msg} ->
        file_path =
          opts
          |> Keyword.fetch!(:mod_file)
          |> Path.relative_to_cwd()

        %{location: {raw_line, _}} =
          opts
          |> Keyword.fetch!(:parsed)
          |> find_function(function.name)

        {file, line} =
          opts
          |> Keyword.fetch!(:manifest)
          |> Manifest.resolve(file_path, raw_line)

        raise CompileError,
          description: msg,
          file: file,
          line: line
    end
  end

  defp find_function(%{code: code}, function) do
    Enum.find(code, fn
      %Zig.Parser.Function{name: ^function} -> true
      %Zig.Parser.Const{name: ^function} -> true
      _ -> false
    end)
  end

  defp validate_args(function, types) do
    function.params
    |> Enum.with_index(1)
    |> Enum.reduce_while(:ok, fn
      {type, index}, :ok ->
        case validate_struct_pub(type, types, "argument #{index}", function.name) do
          :ok ->
            {:cont, :ok}

          error = {:error, _msg} ->
            {:halt, error}
        end
    end)
  end

  # explicitly, all return values that are permitted
  defp validate_return(type) do
    if Type.return_allowed?(type) do
      :ok
    else
      {:error, "functions returning #{type} cannot be nifs"}
    end
  end

  defp validate_struct_pub(%Struct{name: name}, types, location, function) do
    Enum.reduce_while(
      types,
      {:error, "struct `#{name}` (#{location} of function `#{function}`) is not a `pub` struct"},
      fn
        %{name: type_name}, error = {:error, _} ->
          if Atom.to_string(type_name) == name do
            {:halt, :ok}
          else
            {:cont, error}
          end
      end
    )
  end

  defp validate_struct_pub(_, _, _, _), do: :ok
end
