defmodule Mix.Tasks.ZigDoc do
  use Mix.Task

  @shortdoc "Generate documentation with zig files"

  @moduledoc """
  """

  @doc false
  def run(args, config \\ Mix.Project.config(), generator \\ &Zigler.Doc.generate_docs/3) do
    Mix.Tasks.Docs.run(args, config, generator)
  end
end
