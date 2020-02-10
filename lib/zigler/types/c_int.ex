defmodule Zigler.Types.CInt do

  @behaviour Zigler.Types
  def to_beam(parameter) do
    "beam.make_c_int(env, #{parameter})"
  end
end
