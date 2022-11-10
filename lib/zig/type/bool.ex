defmodule Zig.Type.Bool do
  use Zig.Type

  defstruct []

  @type t :: %__MODULE__{}

  def from_json(_), do: %__MODULE__{}

  def to_string(_), do: "bool"
  def to_call(_), do: "bool"

  def marshal_param(_), do: nil
  def marshal_return(_), do: nil

  def param_errors(_) do
    fn index ->
      [
        {{:nif_argument_type_error, index},
         quote do
           case __STACKTRACE__ do
             [{_m, _f, a, _opts}, {m, f, _a, opts} | rest] ->
               new_opts =
                 Keyword.merge(opts,
                   error_info: %{module: __MODULE__, function: :_format_error},
                   zigler_error: %{
                     unquote(index + 1) =>
                       "\n\n     expected: boolean\n     got: #{inspect(Enum.at(a, unquote(index)))}"
                   }
                 )

               :erlang.raise(:error, :badarg, [{m, f, a, new_opts} | rest])

             stacktrace ->
               :erlang.raise(:error, :badarg, stacktrace)
           end
         end}
      ]
    end
  end
end
