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

  def render_elixir_spec(_, _, _) do
    quote do
      float()
    end
  end

  def param_allowed?(_), do: true
  def return_allowed?(_), do: true
  def can_cleanup?(_), do: false

  def render_payload_options(_, _, _), do: Type._default_payload_options()
  def render_return(_, _), do: Type._default_return()
  def marshal_param(_, _, _, _), do: Type._default_marshal()
  def marshal_return(_, _, _), do: Type._default_marshal()
end
