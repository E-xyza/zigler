defmodule Zigler.Doc do
  @spec generate_docs(String.t(), String.t(), Keyword.t()) :: atom
  def generate_docs(project, vsn, options)
      when is_binary(project) and is_binary(vsn) and is_list(options) do
    ExDoc.generate_docs(project, vsn, [retriever: Zigler.Doc.Retriever] ++ options)
  end
end
