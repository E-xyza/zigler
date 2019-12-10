defmodule Zigler.Doc.Retriever do

  @moduledoc false

  require ExDoc.FunctionNode

  def docs_from_dir(source_beam, config) do

    source_dir = if config.project == "zigler" do
      # set the directory to include zig/... instead of what
      # would normally be a source directory for a standard
      # project.
      __ENV__.file
      |> Path.dirname
      |> Path.join("../../zig/beam")
      |> Path.expand
    else
      nil # for now, we'll figure this out later.
    end

    elixir_docs = ExDoc.Retriever.docs_from_dir(source_beam, config)
    zig_docs = Zigler.Doc.Parser.docs_from_dir(source_dir)

    elixir_docs ++ zig_docs
  end
end
