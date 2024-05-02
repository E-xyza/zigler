defmodule Zig.ErrorProng do
  @moduledoc false

  # default parameter errors handling.

  def argument_error_prong(:elixir) do
    quote do
      :error, {:argument_error, index, error_lines} ->
        new_stacktrace =
          case __STACKTRACE__ do
            # this module is only created when the function is being marshalled.
            [{_m, _f, a, _}, {m, f, _a, opts} | rest] ->
              indentation = &["\n     ", List.duplicate("| ", &1)]

              line = 1
              file = "foo"

              # TODO: we want to provide line information on the function here.
              line |> dbg

              new_opts =
                Keyword.merge(opts,
                  error_info: %{module: __MODULE__, function: :_format_error},
                  line: line,
                  file: file,
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
                      |> List.insert_at(0, "\n")
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

  def error_return_prong(:elixir, ignored) do
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
                           line_info: line_info
                         } ->
            {file, line} = __resolve(line_info)
            {String.to_atom(module_str), String.to_atom(fn_str), [:...], [file: file, line: line]}
          end)
          |> Enum.reverse(stacktrace)

        :erlang.raise(:error, type, new_stacktrace)

      :error, {:error, type} ->
        :erlang.raise(:error, type, [])
    end
  end

  def error_return_prong(:erlang, _) do
    ["error:{error, Type, _ExtraStacktrace}:Stacktrace -> erlang:raise(error, Type, Stacktrace)"]
  end
end
