defmodule Zig.Type.Float do
  use Zig.Type, inspect?: true

  defstruct [:bits]

  @type t :: %__MODULE__{bits: 16 | 32 | 64}

  def from_json(%{"bits" => bits}), do: %__MODULE__{bits: bits}

  def to_string(float), do: "f#{float.bits}"
  def to_call(float), do: "f#{float.bits}"


  def param_errors(_, _) do
    fn index ->
      [
        {quote do
           {:argument_error, unquote(index), error_lines}
         end,
         quote do
           case __STACKTRACE__ do
             [{_m, _f, a, _opts}, {m, f, _a, opts} | rest] ->
               new_opts =
                 Keyword.merge(opts,
                   error_info: %{module: __MODULE__, function: :_format_error},
                   zigler_error: %{
                     unquote(index + 1) =>
                       error_lines
                       |> Enum.map(fn error_line ->
                         error_msg = error_line
                         |> Tuple.to_list()
                         |> Enum.map(fn
                           string when is_binary(string) -> string
                           {:inspect, content} -> inspect(content)
                         end)
                         |> List.wrap()
                         |> List.insert_at(0, "\n     ")
                       end)
                       |> List.insert_at(0, "\n")
                       |> IO.iodata_to_binary()
                   }
                 )

               :erlang.raise(:error, :badarg, [{m, f, a, new_opts} | rest])

             stacktrace ->
               # no available stacktrace info
               :erlang.raise(:error, :badarg, stacktrace)
           end
         end}
      ]
    end
  end

  def inspect(type, _opts) do
    import Inspect.Algebra
    concat(["~t(", to_string(type), ")"])
  end

  def return_allowed?(_), do: true
end
