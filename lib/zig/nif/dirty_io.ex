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
  def table_entries(nif) do
    [{nif.type.name, nif.type.arity, :dirty_io}]
  end
end
