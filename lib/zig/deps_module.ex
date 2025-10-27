defmodule Zig.DepsModule do
  @moduledoc false

  @enforce_keys [:dep, :src_mod, :dst_mod]

  defstruct @enforce_keys

  defimpl Zig.Builder do
    def render_build(_, _), do: ""
  end
end
