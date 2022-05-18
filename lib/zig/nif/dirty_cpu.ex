defmodule Zig.Nif.DirtyCpu do
  @moduledoc false

  alias Zig.Nif.{Adapter, Synchronous}

  @behaviour Adapter

  @impl true
  defdelegate zig_adapter(nif, module), to: Synchronous

  @impl true
  def nif_table_entries(nif) do
    """
      e.ErlNifFunc{
        .name = "#{nif.name}",
        .arity = #{nif.arity},
        .fptr = __#{nif.name}_shim__,
        .flags = e.ERL_NIF_DIRTY_JOB_CPU_BOUND,
      },
    """
  end

  @impl true
  defdelegate beam_adapter(nif), to: Synchronous
end
