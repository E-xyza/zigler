defmodule Zig.Type.Float do
  alias Zig.Type
  use Type

  defstruct [:bits]

  @type t :: %__MODULE__{bits: 16 | 32 | 64}

  def parse("f16"), do: %__MODULE__{bits: 16}
  def parse("f32"), do: %__MODULE__{bits: 32}
  def parse("f64"), do: %__MODULE__{bits: 64}

  def from_json(%{"bits" => bits}), do: %__MODULE__{bits: bits}

  def to_call(float), do: "f#{float.bits}"

  def inspect(type, _opts) do
    import Inspect.Algebra
    concat(["~t(", to_string(type), ")"])
  end

  def spec(_, _, _), do: Type.spec(:float)

  def return_allowed?(_), do: true
  def marshals_param?(_), do: false
  def marshals_return?(_), do: false
  def render_payload_options(type, index, _), do: Type._default_payload_options()
  def render_return(type), do: Type._default_return()
end
