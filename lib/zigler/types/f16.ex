defmodule Zigler.Types.F16 do
  @behaviour Zigler.Types
  def to_beam(parameter) do
    "beam.make_f16(env, #{parameter})"
  end
end
