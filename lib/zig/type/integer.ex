defmodule Zig.Type.Integer do
  @moduledoc false

  alias Zig.Type

  use Type

  defstruct [:signedness, :bits]

  @type t :: %__MODULE__{
          signedness: :unsigned | :signed,
          bits: 0..65_535 | :big
        }

  def parse("u" <> number_str = t) do
    %__MODULE__{signedness: :unsigned, bits: get_bits!(number_str, t)}
  end

  def parse("i" <> number_str = t) do
    %__MODULE__{signedness: :signed, bits: get_bits!(number_str, t)}
  end

  def parse(other), do: raise(Zig.Type.ParseError, source: other)

  @impl true
  def render_zig(%{bits: :big}), do: raise("not supported yet")
  def render_zig(%{signedness: :unsigned, bits: bits}), do: "u#{bits}"
  def render_zig(%{signedness: :signed, bits: bits}), do: "i#{bits}"

  @signs %{"signed" => :signed, "unsigned" => :unsigned}
  def from_json(%{"signedness" => s, "bits" => b}) do
    %__MODULE__{signedness: Map.fetch!(@signs, s), bits: b}
  end

  def get_bits!(number_str, full_type) do
    case Integer.parse(number_str) do
      {n, ""} when n in 0..65_535 ->
        n

      {n, ""} ->
        raise Zig.Type.ParseError,
          source: full_type,
          reason: "#{n} is out of range of zig bit lengths"

      _ ->
        raise Zig.Type.ParseError, source: full_type
    end
  end

  def to_call(integer), do: to_string(integer)

  def inspect(type, _opts) do
    import Inspect.Algebra
    concat(["~t(", to_string(type), ")"])
  end

  @impl true
  def marshal_param(type, variable, index, Elixir) do
    marshal_param_elixir(type, variable, index)
  end

  def marshal_param(type, variable, index, :erlang) do
    marshal_param_erlang(type, variable, index)
  end

  defp marshal_param_elixir(%{bits: 0} = type, variable, index) do
    quote bind_quoted: [
            variable: variable,
            name: Type.render_zig(type),
            index: index
          ] do
      unless variable === 0 do
        :erlang.error(
          {:badarg, index, [{"expected: zero (for `#{name}`)"}, {"got: `#{inspect(variable)}`"}]}
        )
      end

      variable
    end
  end

  defp marshal_param_elixir(type, variable, index) do
    max = typemax(type)
    min = typemin(type)

    guards =
      quote bind_quoted: [
              variable: variable,
              name: Type.render_zig(type),
              index: index,
              max: max,
              min: min
            ] do
        unless is_integer(variable) do
          :erlang.error(
            {:badarg, index,
             [{"expected: integer (for `#{name}`)"}, {"got: `#{inspect(variable)}`"}]}
          )
        end

        unless variable >= min do
          :erlang.error(
            {:badarg, index,
             [
               {"expected: integer (for `#{name}`)"},
               {"got: `#{inspect(variable)}`"},
               {"note: out of bounds (#{min}..#{max})"}
             ]}
          )
        end

        unless variable <= max do
          :erlang.error(
            {:badarg, index,
             [
               {"expected: integer (for `#{name}`)"},
               {"got: `#{inspect(variable)}`"},
               {"note: out of bounds (#{min}..#{max})"}
             ]}
          )
        end
      end

    size = _next_power_of_two_ceil(type.bits)

    case type do
      %{bits: bits} when bits <= 64 ->
        []

      %{signedness: :unsigned} ->
        quote do
          unquote(guards)
          unquote(variable) = <<unquote(variable)::unsigned-integer-size(unquote(size))-native>>
        end

      %{signedness: :signed} ->
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

    case type do
      %{bits: 0} ->
        """
        #{variable} = case #{variable} of
          0 -> 0;
          _ -> erlang:error(badarg)
        end,
        """

      %{bits: bits} when bits <= 64 ->
        "#{variable}_m = #{variable},"

      _ ->
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
  end

  @impl true
  def marshal_return(type, variable, Elixir) do
    marshal_return_elixir(type, variable)
  end

  def marshal_return(type, variable, :erlang) do
    marshal_return_erlang(type, variable)
  end

  defp marshal_return_elixir(type, variable) do
    size = _next_power_of_two_ceil(type.bits)

    case type do
      %{bits: bits} when bits <= 64 ->
        variable

      %{signedness: :unsigned} ->
        quote do
          <<result::unsigned-integer-size(unquote(size))-native>> = unquote(variable)
          result
        end

      %{signedness: :signed} ->
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

  @impl true
  def render_elixir_spec(%{bits: 0}, _), do: 0

  def render_elixir_spec(%{signedness: :unsigned, bits: 8}, _opts) do
    quote do
      byte
    end
  end

  def render_elixir_spec(%{signedness: :unsigned} = type, _opts) do
    quote context: Elixir do
      0..unquote(typemax(type))
    end
  end

  def render_elixir_spec(%{signedness: :signed} = type, _opts) do
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

  @impl true
  def get_allowed?(_), do: true
  @impl true
  def make_allowed?(_), do: true
  @impl true
  def in_out_allowed?(_), do: false

  @impl true
  def binary_size(integer) do
    bytes =
      case rem(integer.bits, 8) do
        0 -> div(integer.bits, 8)
        _ -> div(integer.bits, 8) + 1
      end

    {:indirect, bytes}
  end

  @impl true
  def render_accessory_variables(_, _, _), do: Type._default_accessory_variables()

  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()

  @impl true
  def payload_options(_, _), do: Type._default_payload_options()
end
