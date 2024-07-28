defmodule ZiglerTest.MakeBeam do
  def go do
    code =
      :zigler
      |> :code.priv_dir()
      |> Path.join("beam/beam.zig")
      |> File.read!()
      |> String.split("\n")
      |> Enum.reduce({false, []}, fn
        "/// ```" <> _, {true, so_far} -> {false, so_far}
        "///" <> line, {true, so_far} -> {true, [so_far, line, "\n"]}
        "/// ```elixir" <> _, {false, so_far} -> {true, so_far}
        _, state -> state
      end)
      |> elem(1)

    templated = """
    defmodule ZiglerTest.BeamTest do
      use ExUnit.Case, async: true
      use Zig, otp_app: :zigler,
        nifs: [..., yielding_example: [:dirty_cpu]]

    ~Z\"""
    const beam = @import("beam");
    const std = @import("std");
    \"""

    #{code}
    end
    """

    File.write!("test/beam_test.exs", templated)
  end
end
