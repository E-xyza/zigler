defmodule Zig.Parser do
  @moduledoc """
  Zigler still needs a parser to get line/file numbers of some things:

  - function declarations
  - struct declarations
  - documentation comments
  - imports

  In the long run, the need for this will be mitigated by access to ZIR/AIR.
  """

  import NimbleParsec

  context_to_struct = post_traverse(empty(), :context_to_struct)
  whitespace = ignore(ascii_string([?\n, ?\s], min: 1))
  alpha = ascii_char([?a..?z, ?A..?Z, ?_])
  alphanum = ascii_char([?a..?z, ?A..?Z, ?0..?9, ?_])

  identifier =
    alpha
    |> repeat(alphanum)
    |> post_traverse(:coalesce_args)

  function_decl =
    string("fn")
    |> concat(whitespace)
    |> post_traverse(:purge)
    |> concat(identifier)
    |> post_traverse(:record_fun)

  _ = function_decl

  zig_file =
    context_to_struct
    |> optional(whitespace)
    |> repeat(choice([function_decl, whitespace]))

  defparsecp(:parse, zig_file)

  @tested_parsers [identifier: identifier, function_decl: function_decl]

  def parse_decls(content, opts) do
    file_name = Keyword.fetch!(opts, :file)
    case parse(content) do
      {:ok, _, _, context, _, _} ->
        Enum.map(context.items, &Enum.into([file: file_name], &1))

      _ ->
        raise CompileError, description: "something went wrong while parsing #{file_name}"
    end
  end

  defstruct items: []

  # generic tools
  defp context_to_struct(content, args, _context, _, _) do
    {content, args, %__MODULE__{}}
  end

  defp purge(content, _args, context, _, _) do
    {content, [], context}
  end

  defp coalesce_args(rest, args, context, _, _) do
    coalesced =
      args
      |> Enum.reverse()
      |> IO.iodata_to_binary()

    {rest, [coalesced], context}
  end

  # stateful tools for specific parts
  defp record_fun(rest, [identifier], context, {line, _col}, _bytes) do
    new_item = %{
      type: :fun,
      name: identifier,
      line: line
    }
    {rest, [], %{context | items: [new_item | context.items]}}
  end

  # TESTING
  if Mix.env() == :test do
    for {fun, parser} <- @tested_parsers do
      defparsec(fun, concat(context_to_struct, parser))
    end
  end
end
