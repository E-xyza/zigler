defmodule Zigler.Types.I32 do
  def to_beam(parameter) do
    "beam.make_i32(env, #{parameter})"
  end
end
