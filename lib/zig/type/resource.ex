defmodule Zig.Type.Resource do
  @moduledoc false

  alias Zig.Return
  alias Zig.Type
  use Type

  # put in a dummy content that will render resources as "unreachable".
  defstruct name: :unreachable

  @type t :: %__MODULE__{
          name: atom
        }

  def from_json(%{"name" => name}, _) do
    # name must be assigned from the outer calling context
    %__MODULE__{name: String.to_atom(name)}
  end

  def render_elixir_spec(_resource, %Return{as: :binary}) do
    quote do
      binary()
    end
  end

  @impl true
  def render_elixir_spec(_resource, _) do
    quote do
      reference()
    end
  end

  @impl true
  def make_allowed?(_resource), do: true
  @impl true
  def get_allowed?(_), do: true
  @impl true
  def in_out_allowed?(_), do: false

  @impl true
  def binary_size(_), do: nil

  @impl true
  def render_zig(_), do: raise("unreachable")

  @impl true
  def render_accessory_variables(_, _, _), do: Type._default_accessory_variables()

  @impl true
  def payload_options(_, _), do: Type._default_payload_options()

  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()

  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)
  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)
end
