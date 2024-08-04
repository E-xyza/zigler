defmodule Zig.Nif.Synchronous do
  @moduledoc false

  @behaviour Zig.Nif.Concurrency

  alias Zig.Nif
  alias Zig.Nif.Basic

  @impl true
  defdelegate render_elixir(nif), to: Basic

  @impl true
  defdelegate render_erlang(nif), to: Basic

  @impl true
  defdelegate render_zig(nif), to: Basic

  @impl true
  def table_entries(nif) do
    nif
    |> Nif.arities()
    |> Enum.map(&{Basic.entrypoint(nif), &1, nif.name, :synchronous})
  end

  @impl true
  defdelegate resources(nif), to: Basic
end
