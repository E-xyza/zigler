defmodule Zigler.Types.F32 do
  @behaviour Zigler.Types
  def to_beam(parameter) do
    "beam.make_f64(env, #{parameter})"
  end
end
