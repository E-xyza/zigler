defmodule Zigler.Types.U64 do
  @behaviour Zigler.Types
  def to_beam(parameter) do
    "beam.make_u64(env, #{parameter})"
  end
end
