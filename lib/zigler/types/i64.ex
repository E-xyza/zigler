defmodule Zigler.Types.I64 do
  def to_beam(parameter) do
    "beam.make_i64(env, #{parameter})"
  end
end
