defmodule Zig.Type.Resource do
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

  def render_elixir_spec(_resource, context, opts) do
    case {context, opts} do
      {:return, %{as: :binary}} ->
        quote do
          binary()
        end

      {_, _} ->
        quote do
          reference()
        end
    end
  end

  def can_cleanup?(_), do: true

  def return_allowed?(_resource), do: true
  def param_allowed?(_), do: true
end
