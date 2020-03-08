defmodule Zigler.Parser.Resource do

  @moduledoc """
  resource parser object
  """

  @enforce_keys [:name]
  defstruct @enforce_keys ++ [:doc, :cleanup]

  alias Zigler.Parser.ResourceCleanup

  def register_resource_definition(context = %{local: %{name: res}}) do
    # search through global for a resource_cleanup that matches our name parameter.
    # if it's found it then update it.  If it's not found then merely append the
    # resource definition to the global parser state.
    if Enum.any?(context.global, &match?(%ResourceCleanup{for: ^res}, &1)) do
      new_global = replace_cleanup(context.global, context.local)
      %{context | global: new_global}
    else
      %{context | global: [context.local | context.global]}
    end
  end

  # no need for an empty list base case since we know there must exist one match.
  defp replace_cleanup([%ResourceCleanup{for: res, name: cleanup} | rest], r = %{name: res}) do
    [%{r | cleanup: cleanup} | rest]
  end
  defp replace_cleanup([any | rest], res), do: [any | replace_cleanup(rest, res)]

end
