defmodule Zig.Nif.Test do

  @moduledoc """
  Adapter code for test nifs.
  """

  alias Zig.Nif.Adapter

  @behaviour Adapter

  @impl true
  def zig_adapter(nif, _module) do
    # when it is a test:
    """
    export fn __#{Adapter.shim_name nif.name}_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
      beam.test_env = env;
      #{nif.name}() catch return beam.test_error();
      return beam.make_atom(env, "ok");
    }
    """
  end

  @impl true
  def nif_table_entries(nif) do
    """
      e.ErlNifFunc{
        .name = "#{nif.test}",
        .arity = 0,
        .fptr = __#{Adapter.shim_name nif.name}_shim__,
        .flags = 0,
      },
    """
  end

  @impl true
  def beam_adapter(nif) do
    raise_msg = "nif for test #{nif.test} not found"
    raise_code = quote do
      raise unquote(raise_msg)
    end
    {:def, [context: Elixir, import: Kernel],
      [
        {nif.test, [context: Elixir], Elixir},
        [do: raise_code]
      ]}
  end
end
