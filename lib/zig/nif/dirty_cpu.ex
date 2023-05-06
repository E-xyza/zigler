defmodule Zig.Nif.DirtyCpu do
  @moduledoc """
  Dirty Cpu Nifs run dirty, on the cpu
  """

  @behaviour Zig.Nif.Concurrency

  alias Zig.Nif.Basic
  alias Zig.Type.Function

  defdelegate render_elixir(nif), to: Basic
  defdelegate render_erlang(nif), to: Basic
  defdelegate render_zig_code(nif), to: Basic
  defdelegate set_entrypoint(nif), to: Basic

  def table_entries(nif) do
    [
      ~s(.{.name = "#{nif.entrypoint}", .arity = #{nif.function.arity}, .fptr = #{Function.nif_alias_for(nif.function)}, .flags = e.ERL_NIF_DIRTY_JOB_CPU_BOUND})
    ]
  end
end
