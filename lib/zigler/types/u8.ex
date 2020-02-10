defmodule Zigler.Types.U8 do
  @behaviour Zigler.Types
  def to_beam(parameter) do
    "beam.make_u8(env, #{parameter})"
  end
end
