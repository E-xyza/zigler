defmodule Zig.Type.Enum do
  use Zig.Type

  defstruct [:tags, :name]
  @type t :: %__MODULE__{tags: %{optional(atom) => String.t()}, name: String.t()}

  def from_json(%{"tags" => tags, "name" => name}, module) do
    %__MODULE__{
      tags: Map.new(tags, fn {key, val} -> {String.to_atom(key), val} end),
      name: String.trim_leading(name, ".#{module}.")
    }
  end

  def to_string(enum), do: enum.name

  def to_call(enum), do: "nif." <> enum.name

  def inspect(enum, opts) do
    ~s(%Zig.Type.Enum{name: "#{enum.name}", tags: #{Kernel.inspect(enum.tags, opts)}})
  end

  def marshal_param(enum, _) do
    keys =
      enum.tags
      |> Map.values()
      |> MapSet.new()
      |> Macro.escape()

    tags = Macro.escape(enum.tags)

    fn arg, index ->
      quote bind_quoted: [arg: arg, index: index, keys: keys, tags: tags] do
        case arg do
          arg when is_atom(arg) ->
            Map.get(tags, arg) || :erlang.error({:nif_argument_name_error, {index, arg}})

          int when is_integer(int) ->
            int in keys or :erlang.error({:nif_argument_value_error, {index, arg}})
            int

          _ ->
            :erlang.error({:nif_argument_type_error, {index, arg}})
        end
      end
    end
  end

  @arg {:arg, [], Elixir}

  def param_errors(enum, _) do
    type_str = to_string(enum)

    tags =
      enum.tags
      |> Map.keys()
      |> Enum.sort()
      |> Enum.map_join(", ", &inspect/1)

    values =
      enum.tags
      |> Map.values()
      |> Enum.sort()
      |> Enum.map_join(", ", &inspect/1)

    fn index ->
      [
        {{:nif_argument_name_error, {index, @arg}},
         quote do
           case __STACKTRACE__ do
             [{m, f, a, opts} | rest] ->
               new_opts =
                 Keyword.merge(opts,
                   error_info: %{module: __MODULE__, function: :_format_error},
                   zigler_error: %{
                     unquote(index + 1) =>
                       "\n\n     #{inspect(unquote(@arg))} is not a valid atom for #{unquote(type_str)} (should be one of [#{unquote(tags)}])"
                   }
                 )

               :erlang.raise(:error, :badarg, [{m, f, a, new_opts} | rest])

             stacktrace ->
               :erlang.raise(:error, :badarg, stacktrace)
           end
         end},
        {{:nif_argument_value_error, {index, @arg}},
         quote do
           case __STACKTRACE__ do
             [{m, f, a, opts} | rest] ->
               new_opts =
                 Keyword.merge(opts,
                   error_info: %{module: __MODULE__, function: :_format_error},
                   zigler_error: %{
                     unquote(index + 1) =>
                       "\n\n     #{inspect(unquote(@arg))} is not a valid atom for #{unquote(type_str)} (should be one of [#{unquote(values)}])"
                   }
                 )

               :erlang.raise(:error, :badarg, [{m, f, a, new_opts} | rest])

             stacktrace ->
               :erlang.raise(:error, :badarg, stacktrace)
           end
         end},
        {{:nif_argument_type_error, {index, @arg}},
         quote do
           case __STACKTRACE__ do
             [{m, f, a, opts} | rest] ->
               new_opts =
                 Keyword.merge(opts,
                   error_info: %{module: __MODULE__, function: :_format_error},
                   zigler_error: %{
                     unquote(index + 1) =>
                       "\n\n     expected: atom or integer (for #{unquote(type_str)})\n     got: #{inspect(unquote(@arg))}"
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

  def return_allowed?(_), do: true
end
