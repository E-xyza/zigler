defmodule ZiglerTest.Support.Parser do
  # parses .md files and chunks back the triple quoted sections

  @moduledoc false

  defstruct [
    in_docstring?: false,
    docstrings: []
  ]

  @type t :: %__MODULE__{
    in_docstring?: boolean,
    docstrings: [{String.t, non_neg_integer}]
  }

  def resource(file_path) do
    __DIR__
    |> Path.join("../..")
    |> Path.join(file_path)
    |> Path.expand
  end

  def code_blocks(file_path) do
    file_path
    |> File.read!
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.reduce(%__MODULE__{}, fn
      {"```elixir", line}, state = %{in_docstring?: false, docstrings: docstrings} ->
        %{state | in_docstring?: true, docstrings: [{"", line} | docstrings]}
      {"```", _}, state = %{in_docstring?: true} ->
        %{state | in_docstring?: false}
      {docline, _}, state = %{in_docstring?: true, docstrings: [{head, line} | rest]} ->
        %{state | docstrings: [{head <> "\n" <> docline, line} | rest]}
      _, state -> state
      end)
    |> Map.get(:docstrings)
    |> Enum.reverse
  end
end
