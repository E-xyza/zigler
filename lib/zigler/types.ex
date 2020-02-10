defmodule Zigler.Types do
  @spec module_for(String.t) :: module
  def module_for(type) do
    Module.concat(__MODULE__, Macro.camelize(type))
  end

  @callback to_beam(String.t) :: String.t
end
