defmodule Zig.Type.Resource do
  alias Zig.Type
  use Type

  defstruct [:name]

  @type t :: %__MODULE__{name: String.t()}

  def from_json(%{"name" => name}, module) when not is_nil(module) do
    # TODO: use the Zig parser to do this in the future.
    regex = ~r/resource.Resource\(([a-zA-Z0-9_\*\.\[\]\(\)\s]+),sema/
    [match, type] = Regex.run(regex, name)

    new_name = Regex.replace(regex, name, "resource.Resource(#{type},root")
    |> String.replace(".#{module}", "nif")
    |> String.replace("stub_erl_nif", "e")

    %__MODULE__{name: new_name}
  end

  def to_string(resource), do: resource.name

  def to_call(resource), do: resource.name

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
