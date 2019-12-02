defmodule Zigler.Parser do
  @moduledoc """
  contains all code involved in parsing zig code.
  """

  import NimbleParsec

  alias Zigler.Nif
  alias Zigler.Zig

  @alphanumeric [?a..?z, ?A..?Z, ?0..?9, ?_]
  @number [?0..?9]

  #############################################################################
  ## post-traversal functions

  # TODO: consider moving these out into their own module.

  defp store_docstring(_rest, content = ["\n", text | _], context = %{docstring: prev}, _line, _offset) do
    {content, %{context | docstring: [prev, String.trim(text), "\n"]}}
  end
  defp store_docstring(_rest, content = ["\n", text | _], context, _line, _offset) do
    {content, Map.put(context, :docstring, [String.trim(text), "\n"])}
  end

  # NB: nimble_parsec data is operated on in reverse order.
  defp find_nif_info([arity, "/", name, "/// nif: " | _]) do
    {name, String.to_integer(arity)}
  end
  defp find_nif_info(["\n" | rest]), do: find_nif_info(rest)
  defp find_nif_info([?\s | rest]), do: find_nif_info(rest)
  defp find_nif_info(content) do
    raise("parser error #{Enum.reverse(content)}")
  end

  defp store_nif_line_info(_rest, content, context, _line, _offset) do
    {name, arity} = find_nif_info(content)
    {content, Map.put(context, :nif, %{name: name, arity: arity})}
  end

  defp match_function_if_nif(_rest, content = [name | _], context = %{nif: %{name: name}}, _line, _offset) do
    {content, context}
  end
  defp match_function_if_nif(_, [fn_name | _], context = %{nif: %{name: nif_name}}, {line, _}, _) do
    raise CompileError,
      file: context.file,
      line: line,
      description: ~s/nif docstring expecting "#{nif_name}" not adjacent to function (next to "#{fn_name}")/
  end
  defp match_function_if_nif(_rest, content, context, _line, _offset), do: {content, context}

  defp store_parameter(_rest, content, context = %{params: params}, _line, _offset) do
    [type, ":", _] = Enum.reject(content, &(&1 =~ " "))
    {content, %{context | params: params ++ [type]}}
  end
  defp store_parameter(_rest, content, context , _line, _offset) do
    [type, ":", _] = Enum.reject(content, &(&1 =~ " "))
    {content, Map.put(context, :params, [type])}
  end

  defp store_retval(_rest, content = [type | _], context, _line, _offset) do
    {content, Map.put(context, :retval, type)}
  end

  defp save_if_nif(_rest, content, context = %{nif: nif}, {code_line, _}, _offset) do
    # retrieve the various parameters for the nif.
    params = Map.get(context, :params, [])
    doc = Map.get(context, :docstring, nil)
    retval = context.retval
    arity = nif.arity
    found_arity = Enum.count(Zig.adjust_params(params))
    # perform the arity checkt.
    unless arity == found_arity do
      raise CompileError,
        file: context.file,
        line: code_line,
        description: "mismatch of arity declaration, expected #{arity}, got #{found_arity}"
    end

    # build the nif struct that we're going to send back with the code.
    res = %Nif{name: String.to_atom(nif.name),
               arity: arity,
               params: params,
               doc: doc,
               retval: retval}

    {[res | content], context}
  end
  # if it's a plain old function, just ignore all the hullabaloo about nifs.
  defp save_if_nif(_rest, content, context, _, _), do: {content, context}

  defp clear_data(_rest, content, context, _line, _offset) do
    {content, Map.drop(context, [:nif, :docstring, :params, :retval])}
  end

  #############################################################################
  ## nimble_parsec routines

  whitespace = ascii_string([?\s, ?\n], min: 1)

  float_literals = Enum.map(~w(f16 f32 f64), &string/1)
  int_literals = Enum.map(~w(u8 i32 i64 c_int c_long), &string/1)
  array_literals = Enum.map(~w(u8 c_int c_long i32 i64 f16 f32 f64 beam.term), &string/1)
  erlang_literals = Enum.map(~w(?*e.ErlNifEnv e.ErlNifTerm e.ErlNifPid), &string/1)

  type_literals = Enum.map(~w(bool beam.env beam.pid beam.atom beam.term), &string/1)
    ++ float_literals
    ++ int_literals
    ++ erlang_literals

  c_string =
    string("[") |> optional(whitespace)
    |> string("*") |> optional(whitespace)
    |> string("c") |> optional(whitespace)
    |> string("]") |> optional(whitespace)
    |> string("u8")
    |> replace("[*c]u8")

  defp clean_up_array(_rest, [aname | _], context, _line, _offset), do: {["[]#{aname}"], context}

  array_or_string =
    string("[") |> optional(whitespace)
    |> string("]") |> optional(whitespace)
    |> choice(array_literals)
    |> post_traverse(:clean_up_array)

  typeinfo = choice(type_literals ++ [c_string, array_or_string])

  parameter =
    optional(whitespace)
    |> ascii_string(@alphanumeric, min: 1)  # identifier
    |> optional(whitespace)
    |> string(":")
    |> optional(whitespace)
    |> concat(typeinfo)
    |> optional(whitespace)
    |> post_traverse(:store_parameter)

  param_list =
    parameter
    |> repeat(
      string(",")
      |> optional(whitespace)
      |> concat(parameter))

  function_header =
    repeat(ascii_char([?\s]))
    |> string("fn")
    |> concat(whitespace)
    |> (ascii_string(@alphanumeric, min: 1) |> post_traverse(:match_function_if_nif))
    |> string("(")
    |> optional(param_list)
    |> string(")")
    |> optional(whitespace)
    |> concat(typeinfo |> post_traverse(:store_retval))
    |> repeat(ascii_char(not: ?\n))
    |> string("\n")
    |> post_traverse(:save_if_nif)

  nif_line =
    repeat(ascii_char([?\s]))
    |> string("/// nif: ")
    |> ascii_string(@alphanumeric, min: 1)
    |> string("/")
    |> ascii_string(@number, min: 1)
    |> repeat(ascii_char([?\s]))
    |> string("\n")
    |> post_traverse(:store_nif_line_info)
    |> reduce({Enum, :join, []})

  docstring_line =
    repeat(ascii_char([?\s]))
    |> string("///")
    |> lookahead_not(string(" nif:"))
    |> utf8_string([not: ?\n], min: 1)
    |> string("\n")                    # it will be a hard requirement that
    |> post_traverse(:store_docstring) # docstrings end in \n, since zig will
    |> reduce({Enum, :join, []})       # enforce limits on docstring location.

  docstring =
    times(choice([docstring_line, nif_line]), min: 1)
    |> reduce({Enum, :join, []})

  # make intermediate parsing steps testable
  if Mix.env == :test do
    # TODO: tests on parameters
    defparsec :parse_docstring_line, docstring_line
    defparsec :parse_docstring, docstring
    defparsec :parse_nif_line, nif_line
    defparsec :parse_function_header, function_header
  end

  # NB: zig does not allow windows-style crlf line breaks.  However, you can
  # input it and make it go through zig fmt.

  line =
    utf8_string([not: ?\n], min: 1)
    |> string("\n")
    |> post_traverse(:clear_data)
    |> reduce({Enum, :join, []})

  empty_line = string("\n")

  by_line =
    repeat(choice([
      docstring,
      function_header,
      line,
      empty_line
    ]))

  defparsec :zig_by_line, by_line

  @spec parse(String.t, Path.t, non_neg_integer) :: %{code: iodata, nifs: [Zigler.Nif.t]}
  def parse(code, file, line) do
    # prepend a comment saving the file and line metadata.
    marker_comment = "// #{file} line: #{line}\n"

    {:ok, new_code, _, _, _, _} = zig_by_line(code, line: line, context: %{file: file})

    Enum.reduce(new_code, %{code: marker_comment, nifs: [], imports: []}, fn
      res = %Zigler.Nif{}, acc = %{nifs: nifs} ->
        %{acc | nifs: [res | nifs]}
      any, acc = %{code: code} ->
        %{acc | code: [code, any]}
    end)
  end
end
