defmodule Zigler.Types.CUlong do

  @behaviour Zigler.Types
  def to_beam(parameter) do
    "beam.make_c_ulong(env, #{parameter})"
  end
end
