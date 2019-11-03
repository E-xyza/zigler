defmodule Zigler.Code do
  @moduledoc """
  this datastructure represents a stretch of zigler code that is inside of a
  sigil_Z stretch.  The following fields are used:

  - `:code` - textual code that will appear inside the assembled `.zig` file
    to be compiled by Zigler.
  - `:nifs` - structured information about the nifs that appear inside this
    code block.
  - `:file` - elixir `.ex` or `.exs` file associated with this code block.
  - `:line` - line in the above file where the sigil_Z code starts.
  - `:imports` - a recursively generated list of files to be imported.

  """

  @enforce_keys [:file, :line]
  defstruct @enforce_keys ++ [code: "", nifs: []]

  @type t :: %__MODULE__{
    code: iodata,
    nifs: [Zigler.Nif.t],
    file: Path.t,
    line: non_neg_integer
  }

  alias Zigler.Parser

  @spec from_string(code::String.t, file::Path.t, line::non_neg_integer) :: t
  def from_string(code, file, line) do
    parsed = Parser.parse(code, file, line)

    %__MODULE__{
      code: parsed.code,
      file: file,
      line: line,
      nifs: parsed.nifs
    }
  end
end
