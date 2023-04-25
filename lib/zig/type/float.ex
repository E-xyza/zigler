defmodule Zig.Type.Float do
  use Zig.Type, inspect?: true

  defstruct [:bits]

  @type t :: %__MODULE__{bits: 16 | 32 | 64}

  def from_json(%{"bits" => bits}), do: %__MODULE__{bits: bits}

  def to_string(float), do: "f#{float.bits}"
  def to_call(float), do: "f#{float.bits}"

  def inspect(type, _opts) do
    import Inspect.Algebra
    concat(["~t(", to_string(type), ")"])
  end

  def return_allowed?(_), do: true
end
