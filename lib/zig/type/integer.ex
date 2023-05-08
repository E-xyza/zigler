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

  def marshals_param?(%{bits: bits}), do: bits > 64
  def marshals_return?(%{bits: bits}), do: bits > 64

  def marshal_param(type, variable, index, :elixir) do
    marshal_param_elixir(type, variable, index)
  end

  def marshal_param(type, variable, index, :erlang) do
    marshal_param_erlang(type, variable, index)
  end

  defp marshal_param_elixir(type, variable, index) do
    max = typemax(type)
    min = typemin(type)

    guards =
      quote bind_quoted: [
              variable: variable,
              name: to_string(type),
              index: index,
              max: max,
              min: min
            ] do
        unless is_integer(variable) do
          :erlang.error(
            {:argument_error, index,
             [{"expected: integer (for `#{name}`)"}, {"got: `#{inspect(variable)}`"}]}
          )
        end

        unless variable >= min do
          :erlang.error(
            {:argument_error, index,
             [
               {"expected: integer (for `#{name}`)"},
               {"got: `#{inspect(variable)}`"},
               {"note: out of bounds (#{min}..#{max})"}
             ]}
          )
        end

        unless variable <= max do
          :erlang.error(
            {:argument_error, index,
             [
               {"expected: integer (for `#{name}`)"},
               {"got: `#{inspect(variable)}`"},
               {"note: out of bounds (#{min}..#{max})"}
             ]}
          )
        end
      end

    size = _next_power_of_two_ceil(type.bits)

    case type.signedness do
      :unsigned ->
        quote do
          unquote(guards)
          unquote(variable) = <<unquote(variable)::unsigned-integer-size(unquote(size))-native>>
        end

      :signed ->
        quote do
          unquote(guards)
          unquote(variable) = <<unquote(variable)::signed-integer-size(unquote(size))-native>>
        end
    end
  end

  defp marshal_param_erlang(type, variable, _index) do
    max = typemax(type)
    min = typemin(type)

    size = _next_power_of_two_ceil(type.bits)

    """
    #{variable}_m = case #{variable} of
      X when not is_integer(X) ->
        erlang:error(badarg);
      X when X < #{min} ->
        erlang:error(badarg);
      X when X > #{max} ->
        erlang:error(badarg);
      X -> <<X:#{size}/native-#{type.signedness}-integer-unit:1>>
    end,
    """
  end

  def marshal_return(type, variable, :elixir) do
    marshal_return_elixir(type, variable)
  end

  def marshal_return(type, variable, :erlang) do
    marshal_return_erlang(type, variable)
  end

  defp marshal_return_elixir(type, variable) do
    size = _next_power_of_two_ceil(type.bits)

    case type.signedness do
      :unsigned ->
        quote do
          <<result::unsigned-integer-size(unquote(size))-native>> = unquote(variable)
          result
        end

      :signed ->
        quote do
          <<result::signed-integer-size(unquote(size))-native>> = unquote(variable)
          result
        end
    end
  end

  defp marshal_return_erlang(type, variable) do
    size = _next_power_of_two_ceil(type.bits)

    """
    <<NumberResult:#{size}/native-#{type.signedness}-integer>> = #{variable},
    NumberResult
    """
  end

  def _next_power_of_two_ceil(bits), do: _next_power_of_two_ceil(bits, 1, true)

  defp _next_power_of_two_ceil(bits, so_far, all_zeros) do
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

  def spec(%{bits: 0}, _, _), do: 0

  def spec(type = %{signedness: :unsigned}, _, _opts) do
    quote context: Elixir do
      0..unquote(typemax(type))
    end
  end

  def spec(type = %{signedness: :signed}, _, _opts) do
    neg_typemin = -typemin(type)

    quote context: Elixir do
      -unquote(neg_typemin)..unquote(typemax(type))
    end
  end

  # note that we're currently not using this function for unsigned ints, which we know is zero
  defp typemin(%{signedness: :signed, bits: 0}), do: 0
  defp typemin(%{signedness: :signed, bits: bits}), do: -Bitwise.<<<(1, bits - 1)
  defp typemin(%{signedness: :unsigned}), do: 0

  defp typemax(%{bits: 0}), do: 0
  defp typemax(%{signedness: :unsigned, bits: bits}), do: Bitwise.<<<(1, bits) - 1
  defp typemax(%{signedness: :signed, bits: 1}), do: 0
  defp typemax(%{signedness: :signed, bits: bits}), do: Bitwise.<<<(1, bits - 1) - 1

  def return_allowed?(_), do: true
end
