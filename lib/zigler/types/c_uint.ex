defmodule Zigler.Types.CUint do

  @behaviour Zigler.Types
  def to_beam(parameter) do
    "beam.make_c_uint(env, #{parameter})"
  end
end
