defmodule Zig.Type.Float do
  use Zig.Type

  defstruct [:bits]

  @type t :: %__MODULE__{bits: 16 | 32 | 64}

  def from_json(%{"bits" => bits}), do: %__MODULE__{bits: bits}

  def to_string(float), do: "f#{float.bits}"
  def to_call(float), do: "f#{float.bits}"

  def inspect(type, _opts) do
    concat(["~t(", to_string(type), ")"])
  end

  def marshal_param(_, _), do: nil
  def marshal_return(_, _), do: nil
  def param_errors(_, _), do: nil
end
