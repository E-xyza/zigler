defmodule Zigler.Parser.Unit do
  @moduledoc """
  parses zig code and converts test blocks to test functions
  """

  defstruct tests: []

  import NimbleParsec

  whitespace = ascii_string([?\s, ?\n], min: 1)
  blankspace = ascii_string([?\s], min: 1)

  init_context = empty()
  |> post_traverse(:init_context)

  # test declaration
  test_decl = ignore(
    string("test")
    |> concat(whitespace)
    |> string("\""))
  |> ascii_string([not: ?\"], min: 1)
  |> ignore(string("\""))
  |> post_traverse(:parse_test_decl)

  if Mix.env == :test do
    defparsec :test_decl_parser, concat(init_context, test_decl)
  end

  test_line = optional(blankspace)
  |> concat(test_decl)
  |> concat(whitespace)
  |> string("{")
  |> optional(ascii_string([not: ?\n], min: 1))
  |> string("\n")

  blank_line = optional(ascii_string([not: ?\n], min: 1))
  |> string("\n")

  unit_code = init_context
  |> repeat(choice([
    test_line,
    blank_line]))

  defparsec :unit_parser, unit_code

  # ninjas in a context module for the context map.
  defp init_context(_, _, _, _, _) do
    {[], %__MODULE__{}}
  end

  defp parse_test_decl(_rest, [test_name], context, _line, _offest) do
    test_nif = new_nif(test_name)
    {["fn #{test_nif.name}() !void"],
      %{context | tests: [test_nif | context.tests]}}
  end

  alias Zigler.Parser.Nif
  defp new_nif(name) do
    %Nif{
      name: "test_#{Zigler.Unit.name_to_hash name}",
      test: name,
      arity: 0,
      args: [],
      retval: "!void"
    }
  end

end
