defmodule Zig.Type.Integer do
  use Zig.Type, inspect?: true

  defstruct [:signedness, :bits]

  @type t :: %__MODULE__{
          signedness: :unsigned | :signed,
          bits: 0..65535 | :big
        }

  def parse(t = "u" <> number_str) do
    %__MODULE__{signedness: :unsigned, bits: get_bits!(number_str, t)}
  end

  def parse(t = "i" <> number_str) do
    %__MODULE__{signedness: :signed, bits: get_bits!(number_str, t)}
  end

  def parse(other), do: raise(Zig.Type.ParseError, source: other)

  @signs %{"signed" => :signed, "unsigned" => :unsigned}
  def from_json(%{"signedness" => s, "bits" => b}) do
    %__MODULE__{signedness: Map.fetch!(@signs, s), bits: b}
  end

  def get_bits!(number_str, full_type) do
    case Integer.parse(number_str) do
      {n, ""} when n in 0..65535 ->
        n

      {n, ""} ->
        raise Zig.Type.ParseError,
          source: full_type,
          reason: "#{n} is out of range of zig bit lengths"

      _ ->
        raise Zig.Type.ParseError, source: full_type
    end
  end

  def to_string(integer) do
    case integer.signedness do
      :unsigned -> "u#{integer.bits}"
      :signed -> "i#{integer.bits}"
    end
  end

  def to_call(integer), do: to_string(integer)

  def inspect(type, _opts) do
    import Inspect.Algebra
    concat(["~t(", to_string(type), ")"])
  end

  def marshal_param(type = %{signedness: :unsigned, bits: bits}, _) when bits > 64 do
    size = _next_power_of_two_ceil(bits)
    max = typemax(type)

    fn arg, index ->
      quote bind_quoted: [arg: arg, size: size, index: index, max: max] do
        unless is_integer(arg), do: :erlang.error({:nif_argument_type_error, {index, arg}})
        unless arg >= 0, do: :erlang.error({:nif_argument_range_error, {index, arg}})
        unless arg <= max, do: :erlang.error({:nif_argument_range_error, {index, arg}})
        <<arg::unsigned-integer-size(size)-native>>
      end
    end
  end

  def marshal_param(type = %{signedness: :signed, bits: bits}, _) when bits > 64 do
    size = _next_power_of_two_ceil(bits)
    max = typemax(type)
    min = typemin(type)

    fn arg, index ->
      quote bind_quoted: [arg: arg, size: size, index: index, min: min, max: max] do
        unless is_integer(arg), do: :erlang.error({:nif_argument_type_error, {index, arg}})
        unless arg >= min, do: :erlang.error({:nif_argument_range_error, {index, arg}})
        unless arg <= max, do: :erlang.error({:nif_argument_range_error, {index, arg}})
        <<arg::signed-integer-size(size)-native>>
      end
    end
  end

  def marshal_param(_, _), do: nil

  def marshal_return(%{signedness: :unsigned, bits: bits}, _) when bits > 64 do
    size = _next_power_of_two_ceil(bits)

    fn arg ->
      quote bind_quoted: [arg: arg, size: size] do
        <<result::unsigned-integer-size(size)-native>> = arg
        result
      end
    end
  end

  def marshal_return(%{signedness: :signed, bits: bits}, _) when bits > 64 do
    size = _next_power_of_two_ceil(bits)

    fn arg ->
      quote bind_quoted: [arg: arg, size: size] do
        <<result::signed-integer-size(size)-native>> = arg
        result
      end
    end
  end

  def marshal_return(_, _), do: nil

  # TODO: refactor these out into a common module.
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

  def _next_power_of_two_ceil(bits), do: _next_power_of_two_ceil(bits, 1, true)

  def _next_power_of_two_ceil(bits, so_far, all_zeros) do
    import Bitwise
    shifted = bits >>> 1

    case {shifted, all_zeros} do
      {1, true} ->
        1 <<< so_far

      {1, false} ->
        2 <<< so_far

      _ ->
        all_zeros = all_zeros && (bits &&& 1) == 0
        _next_power_of_two_ceil(shifted, so_far + 1, all_zeros)
    end
  end

  defp typemin(%{bits: 0}), do: 0
  defp typemin(%{signedness: :unsigned}), do: 0
  defp typemin(%{bits: bits}), do: -Bitwise.<<<(1, bits - 1)

  defp typemax(%{bits: 0}), do: 0
  defp typemax(%{signedness: :unsigned, bits: bits}), do: Bitwise.<<<(1, bits) - 1
  defp typemax(%{signedness: :signed, bits: 1}), do: 0
  defp typemax(%{signedness: :signed, bits: bits}), do: Bitwise.<<<(1, bits - 1) - 1

  def return_allowed?(_), do: true
end
