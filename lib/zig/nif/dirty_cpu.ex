defmodule Zig.Nif.DirtyCpu do
  @moduledoc false

  @behaviour Zig.Nif.Concurrency

  alias Zig.Nif.Basic

  @impl true
  defdelegate render_elixir(nif, overrides), to: Basic

  @impl true
  defdelegate render_erlang(nif), to: Basic

  @impl true
  defdelegate render_zig(nif), to: Basic

  @impl true
  def table_entries(nif) do
    [{Basic.entrypoint(nif), nif.signature.arity, nif.name, :dirty_cpu}]
  end

  @impl true
  defdelegate resources(nif), to: Basic
end
