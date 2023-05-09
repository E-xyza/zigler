defmodule Zig.Type.Resource do
  alias Zig.Type
  use Type

  defstruct [:name]

  @type t :: %__MODULE__{name: String.t()}

  def from_json(%{"name" => name, "payload" => payload}, module) do
    # TODO: use the Zig parser to do this in the future.
    name =
      ~r/resource.Resource\(([a-zA-Z0-9_\.\[\]\(\)\s]+),sema/
      |> Regex.replace(name, "Resource(#{payload},root")
      |> String.replace(".#{module}", "nif")
      |> String.replace_prefix("", "beam.")
      |> String.replace("(threads", "(beam.threads")
      |> String.replace("stub_erl_nif", "e")

    %__MODULE__{name: name}
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
