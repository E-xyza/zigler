defmodule Zigler.Doc.Retriever do

  @moduledoc """
  decides what features should be documented on `zig doc`.

  may be in flux.
  """

  require ExDoc.FunctionNode

  alias Zigler.Doc.Parser

  @odd_modules [:elixir_bootstrap]

  def docs_from_dir(source_beam, config) do

    source_dirs = if config.project == "zigler" do
      # set the directory to include zig/... instead of what
      # would normally be a source directory for a standard
      # project.
      [:zigler
       |> :code.priv_dir
       |> Path.join("beam")
       |> Path.expand]
    else
      :erlang.loaded()
      |> Enum.filter(&function_exported?(&1, :__info__, 1))
      |> Enum.reject(&(&1 in @odd_modules))
      |> Enum.flat_map(&(&1.__info__(:attributes)[:zig_src_dir] || []))
      |> Enum.uniq
    end

    elixir_docs = ExDoc.Retriever.docs_from_dir(source_beam, config)
    zig_docs = Enum.flat_map(source_dirs, &Parser.docs_from_dir(&1, config))

    elixir_docs ++ zig_docs
  end
end
