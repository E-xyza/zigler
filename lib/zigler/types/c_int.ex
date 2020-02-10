defmodule Zigler.Types.CInt do
  def to_beam(parameter) do
    "beam.make_c_int(env, #{parameter})"
  end
end
