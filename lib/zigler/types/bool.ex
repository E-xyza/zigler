defmodule Zigler.Types.Bool do

  @behaviour Zigler.Types
  def to_beam(parameter) do
    "beam.make_bool(env, #{parameter})"
  end
end
