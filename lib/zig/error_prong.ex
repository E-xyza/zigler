defmodule Zig.ErrorProng do
  # default parameter errors handling.

  def argument_error_prong(:elixir) do
    quote do
      :error, {:argument_error, index, error_lines} ->
        new_stacktrace =
          case __STACKTRACE__ do
            [{_m, _f, a, _opts}, {m, f, _a, opts} | rest] ->
              indentation = &["\n     ", List.duplicate("| ", &1)]

              new_opts =
                Keyword.merge(opts,
                  error_info: %{module: __MODULE__, function: :_format_error},
                  zigler_error: %{
                    index + 1 =>
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
                                "#{inspect(content)}"

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

  def error_return_prong(:elixir) do
    quote do
      _, _ -> :bar
    end
  end
end
