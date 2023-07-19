defmodule Zig.Nif.DirtyIo do
  @moduledoc """
  Dirty Cpu Nifs run dirty, on the cpu
  """

  @behaviour Zig.Nif.Concurrency

  alias Zig.Nif.Basic

  @impl true
  defdelegate render_elixir(nif), to: Basic

  @impl true
  defdelegate render_erlang(nif), to: Basic

  @impl true
  defdelegate render_zig(nif), to: Basic

  @impl true
  def table_entries(%{type: type} = nif) do
    [{Basic.entrypoint(nif), type.arity, type.name, :dirty_io}]
  end

  @impl true
  defdelegate resources(nif), to: Basic
end
