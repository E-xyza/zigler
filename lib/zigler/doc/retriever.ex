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
      |> Path.join("../../../zig/beam")
      |> Path.expand
    else
      :erlang.loaded()
      |> Enum.filter(&function_exported?(&1, :__info__, 1))
      |> Enum.flat_map(fn mod ->
        src_dir = mod.__info__(:attributes)[:zig_src_dir]
        if src_dir, do: [src_dir], else: []
      end)
      |> IO.inspect(label: "24")
      nil # for now, we'lllo figure this out later.
    end

    elixir_docs = ExDoc.Retriever.docs_from_dir(source_beam, config)
    zig_docs = Zigler.Doc.Parser.docs_from_dir(source_dir)

    elixir_docs ++ zig_docs
  end
end
