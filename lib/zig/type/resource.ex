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

  def spec(_resource, context, opts) do
    case {context, Keyword.get(opts, :type)} do
      {:return, :binary} ->
        Type.spec(:binary)

      {_, _} ->
        Type.spec(:reference)
    end
  end

  def return_allowed?(_resource), do: true
end
