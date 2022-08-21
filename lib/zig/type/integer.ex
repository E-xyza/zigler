defmodule Zig.Type.Integer do
  use Zig.Type

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

  def inspect(type, _opts) do
    concat(["~t(", to_string(type), ")"])
  end

  def marshal_elixir(%{signedness: :unsigned, bits: 64}) do
    fn arg ->
      quote bind_quoted: [arg: arg] do
        <<arg::signed-integer-size(64)>> = <<arg::64>>
        arg
      end
    end
  end

  def marshal_elixir(_), do: nil

  def marshal_zig(%{signedness: :unsigned, bits: 64}) do
    fn arg ->
      quote bind_quoted: [arg: arg] do
        <<arg::64>> = <<arg>>
      end
    end
  end

  def marshal_zig(_), do: nil
end
