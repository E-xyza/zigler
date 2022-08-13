defmodule Zig.Type.Integer do
  use Zig.Type

  defstruct [:signedness, :bits]
  @type t :: %__MODULE__{
    signedness: :unsigned | :signed,
    bits: 0..65535
  }

  def parse("u" <> number_str) do
    %__MODULE__{signedness: :unsigned, bits: get_bits!(number_str)}
  end

  def parse("i" <> number_str) do
    %__MODULE__{signedness: :signed, bits: get_bits!(number_str)}
  end

  def parse(other), do: raise Zig.Type.ParseError, source: other

  def get_bits!(number_str) do
    case Integer.parse(number_str) do
      {n, ""} when n in 0..65535 ->
        n
      {n, ""} ->
        raise Zig.Type.ParseError, source: number_str, reason: "#{n} is out of range of zig bit lengths"
      _ ->
        raise Zig.Type.ParseError, source: number_str
    end
  end

  def to_string(integer) do
    case integer.signedness do
      :unsigned -> "u#{integer.bits}"
      :signed -> "i#{integer.bits}"
    end
  end

  def inspect(type, _opts) do
    concat(["~t(", to_string(type), ")"])
  end
end
