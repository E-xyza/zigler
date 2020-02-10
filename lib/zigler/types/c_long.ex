defmodule Zigler.Types.CLong do
  def to_beam(parameter) do
    "beam.make_c_long(env, #{parameter})"
  end
end
