defmodule Zig.ErrorProng do
  @moduledoc false

  # default parameter errors handling.

  def argument_error_prong(:elixir, file, line) do
    newline = Zig.Command.newline()

    quote do
      :error, {:argument_error, index, error_lines} ->
        new_stacktrace =
          case __STACKTRACE__ do
            # this module is only created when the function is being marshalled.
            [{_m, _f, a, _}, {m, f, _a, opts} | rest] ->
              indentation = &["\n     ", List.duplicate("| ", &1)]

              # TODO: make sure line and file are assigned here.

              new_opts =
                Keyword.merge(opts,
                  error_info: %{module: __MODULE__, function: :_format_error},
                  line: unquote(line),
                  file: unquote(file),
                  zigler_error: %{
                    (index + 1) =>
                      error_lines
                      |> Enum.reduce({[], 0}, fn
                        :enter, {so_far, indents} ->
                          {so_far, indents + 1}

                        error_line, {so_far, indents} ->
                          error_msg =
                            error_line
                            |> Tuple.to_list()
                            |> Enum.map(fn
                              list when is_list(list) ->
                                list

                              string when is_binary(string) ->
                                string

                              {:inspect, content} ->
                                "#{inspect(content, custom_options: [sort_maps: true])}"

                              {:typename, typename} ->
                                String.replace(typename, ".#{__MODULE__}.", "")
                            end)
                            |> List.wrap()
                            |> List.insert_at(0, indentation.(indents))

                          {[error_msg | so_far], indents}
                      end)
                      |> elem(0)
                      |> Enum.reverse()
                      |> List.insert_at(0, unquote(newline))
                      |> IO.iodata_to_binary()
                  }
                )

              [{m, f, a, new_opts} | rest]

            stacktrace ->
              stacktrace
          end

        :erlang.raise(:error, :badarg, new_stacktrace)
    end
  end

  def argument_error_prong(:erlang) do
    [
      "error:{error, badarg, _ExtraStacktrace}:Stacktrace -> erlang:raise(error, badarg, Stacktrace)"
    ]
  end

  def return_error_prong(:elixir, ignored) do
    stacktrace_prep =
      case ignored do
        [] ->
          quote do
            __STACKTRACE__
          end

        list when is_list(list) ->
          quote do
            Enum.reject(
              __STACKTRACE__,
              &match?({__MODULE__, ignore, _, _} when ignore in unquote(ignored), &1)
            )
          end
      end

    quote do
      :error, {:error, type, extra_stacktrace} ->
        stacktrace = unquote(stacktrace_prep)

        new_stacktrace =
          extra_stacktrace
          |> Enum.map(fn %{
                           compile_unit_name: module_str,
                           symbol_name: fn_str,
                           source_location: source_location
                         } ->
            {file, line} = __resolve(source_location)
            {String.to_atom(module_str), String.to_atom(fn_str), [:...], [file: file, line: line]}
          end)
          |> Enum.reverse(stacktrace)

        :erlang.raise(:error, type, new_stacktrace)

      :error, {:error, type} ->
        :erlang.raise(:error, type, [])
    end
  end

  def return_error_prong(:erlang, _) do
    ["error:{error, Type, _ExtraStacktrace}:Stacktrace -> erlang:raise(error, Type, Stacktrace)"]
  end
end
