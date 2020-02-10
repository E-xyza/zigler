defmodule Zigler.Types.I32 do

  @behaviour Zigler.Types
  def to_beam(parameter) do
    "beam.make_i32(env, #{parameter})"
  end
end
