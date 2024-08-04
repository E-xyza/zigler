defmodule Zig.Type.Float do
  @moduledoc false

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

  @impl true
  def render_zig(float), do: "f#{float.bits}"

  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()

  @impl true
  def render_elixir_spec(_, _) do
    quote do
      float()
    end
  end

  @impl true
  def get_allowed?(_), do: true
  @impl true
  def make_allowed?(_), do: true
  @impl true
  def in_out_allowed?(_), do: false

  @impl true
  def binary_size(float), do: {:indirect, div(float.bits, 8)}

  @impl true
  def render_accessory_variables(_, _, _), do: Type._default_accessory_variables()
  @impl true
  def payload_options(_, _), do: Type._default_payload_options()
  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)
  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)
end
