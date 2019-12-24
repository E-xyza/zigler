defmodule Mix.Tasks.ZigDoc do
  use Mix.Task

  @shortdoc "Generate documentation with zig files"

  @moduledoc """
  Used to generate documentation from your zig code and incorporate it into
  your Elixir documentation.

  Generally speaking, zig structs will be documented as Elixir modules and
  zig functions will be incorporated as Elixir functions.

  If you want to make the zig docgen process override the Elixir `mix docs`
  task, add this to your `mix.exs`:

  ```
  def project do
    [
      ...
      aliases: [docs: "zig_doc"],
      ...
    ]
  end
  ```
  """

  alias Mix.Tasks.Docs

  @doc false
  def run(args, config \\ Mix.Project.config(), generator \\ &Zigler.Doc.generate_docs/3) do
    Docs.run(args, config, generator)
  end
end
