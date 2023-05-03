defmodule Zig.Type.Resource do
  alias Zig.Type
  use Type

  defstruct [:payload, :name]

  @type t :: %__MODULE__{payload: Type.t(), name: String.t()}

  def from_json(%{"payload" => payload, "name" => name}, module) do
    payload = Type.from_json(payload, module)

    # TODO: use the Zig parser to do this in the future.

    name =
      ~r/resource.Resource\(([a-zA-Z0-9_\.\[\]\(\)]+),sema/
      |> Regex.replace(name, "Resource(#{Type.to_call(payload)},root")
      |> String.replace(".#{module}", "nif")

    %__MODULE__{payload: payload, name: name}
  end

  def to_string(resource), do: resource.name

  def to_call(resource), do: resource.name

  def spec(_resource, context, opts) do
    case {context, Keyword.fetch!(opts, :type)} do
      {:return, :binary} ->
        Type.spec(:binary)

      {_, _} ->
        Type.spec(:reference)
    end
  end

  def return_allowed?(_resource), do: true
end
