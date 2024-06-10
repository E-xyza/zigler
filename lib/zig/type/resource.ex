defmodule Zig.Type.Resource do
  alias Zig.Return
  alias Zig.Type
  use Type

  # put in a dummy content that will render resources as "unreachable".
  defstruct name: :unreachable

  @type t :: %__MODULE__{
          name: atom
        }

  def from_json(_, _) do
    # name must be assigned from the outer calling context
    %__MODULE__{}
  end

  def render_elixir_spec(_resource, %Return{as: :binary}) do
    quote do
      binary()
    end
  end

  def render_elixir_spec(_resource, _) do
    quote do
      reference()
    end
  end

  def can_cleanup?(_), do: true

  def make_allowed?(_resource), do: true
  def get_allowed?(_), do: true
  def binary_size(_), do: nil

  def render_payload_options(_, _, _), do: Type._default_payload_options()
  def marshal_param(_, _, _, _), do: Type._default_marshal()
  def marshal_return(_, _, _), do: Type._default_marshal()
end
