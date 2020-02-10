defmodule Zigler.Types.U16 do
  @behaviour Zigler.Types
  def to_beam(parameter) do
    "beam.make_u16(env, #{parameter})"
  end
end
