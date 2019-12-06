defmodule Zigler.Doc.Parser do
  def docs_from_dir(dir) do
    dir
    |> File.ls!
    |> Enum.filter(&Regex.match?(~r/\.zig$/, &1))  # only read .zig files
    |> Enum.flat_map(&moduledocs_from_file(dir, &1))
  end

  def moduledocs_from_file(dir, file) do
    file_path = Path.join(dir, file)
    base = Path.basename(file_path, ".zig")
    rel_path = Path.relative_to(file_path, File.cwd!)

    init_mod =  %ExDoc.ModuleNode{
      doc_line: 1,
      function_groups: ["Functions", "Values"],
      group: "Zig Structs",
      type: :module,
      id: base,
      module: String.to_atom(base),
      source_path: rel_path,
      title: base
    }

    # a hacky way of sneaking information into the nimble_parsec
    # parser.
    Process.put(:"$init_mod", init_mod)

    {:ok, _, _, parsed_module, _, _} = file_parser(File.read!(file_path))

    # split out the error modules from the parsed module.
    {types, exceptions} = Enum.split_with(
      parsed_module.typespecs,
      &(match?(%ExDoc.TypeNode{}, &1)))

    [%{parsed_module | typespecs: types} | exceptions]
  end

  import NimbleParsec

  whitespace = ascii_string([?\s, ?\n], min: 1)

  docstring_line =
    optional(whitespace)
    |> string("///")
    |> lookahead_not(ascii_char([?/]))
    |> optional(utf8_string([not: ?\n], min: 1))
    |> string("\n")
    |> reduce({Enum, :join, []})

  docstring =
    times(docstring_line, min: 1)
    |> reduce({:strip_docline, []})

  line =
    optional(utf8_string([not: ?\n], min: 1))
    |> string("\n")

  identifier =
    ascii_char([?a..?z, ?A..?Z, ?_])
    |> optional(ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_], min: 1))
    |> reduce({IO, :iodata_to_binary, []})

  type_head =
    optional(whitespace)
    |> ignore(
      string("pub")
      |> concat(whitespace)
      |> string("const")
      |> concat(whitespace))
    |> concat(identifier)
    |> ignore(
      optional(whitespace)
      |> string("=")
      |> optional(whitespace))
    |> lookahead_not(string("error"))
    |> concat(ascii_string([not: ?;], min: 1))
    |> ignore(string(";"))
    |> tag(:type)

  error_head =
    optional(whitespace)
    |> ignore(
      string("pub")
      |> concat(whitespace)
      |> string("const")
      |> concat(whitespace))
    |> concat(identifier)
    |> ignore(
      optional(whitespace)
      |> string("=")
      |> optional(whitespace)
      |> string("error")
      |> optional(whitespace)
      |> string("{"))
    |> repeat(
      ignore(optional(whitespace))
      |> concat(identifier)
      |> ignore(optional(whitespace |> string(","))))
    |> ignore(
      optional(whitespace)
      |> string("}"))
    |> tag(:err)

  fn_head =
    optional(whitespace)
    |> ignore(
      string("pub")
      |> concat(whitespace)
      |> string("fn")
      |> concat(whitespace))
    |> concat(identifier)
    |> concat(ascii_string([not: ?{], min: 1))
    |> ignore(string("{"))
    |> tag(:fn)

  typed_docstring =
    docstring
    |> choice([type_head, error_head, fn_head])
    |> post_traverse(:typed_docstring)

  file_parser =
    empty()
    |> post_traverse(:initialize)
    |> optional(
      empty()
      |> concat(docstring)
      |> post_traverse(:moduledoc)
    )
    |> repeat(choice([
      typed_docstring,
      line
    ]))

  defparsec(:file_parser, file_parser)

  defp strip_docline(line) do
    line
    |> Enum.map(fn line ->
      "///" <> rest = String.trim(line)
      String.trim(rest)
    end)
    |> Enum.join("\n")
  end

  defp initialize(_rest, content, _context, _line , _offset) do
    {content, Process.get(:"$init_mod")}
  end

  defp moduledoc(_rest, [content], context, _line, _offset) do
    {[], struct(context, doc: content)}
  end
  defp moduledoc(_rest, [], context, _line, _offset) do
    {[], struct(context, doc: nil)}
  end

  head_parser =
    ignore(
      optional(whitespace)
      |> string("("))
    |> repeat(
      ignore(optional(whitespace))
      |> choice([
        ignore(string("comptime") |> concat(whitespace))
        |> concat(identifier)
        |> tag(:comptime),
        identifier
      ])
      |> ignore(
        optional(whitespace)
        |> string(":")
        |> optional(whitespace))
      |> ascii_string([not: ?,, not: ?\n, not: ?)], min: 1)
      |> ignore(optional(string(","))))
    |> ignore(string(")"))

  defparsec(:head_parser, head_parser)

  defp typed_docstring(_rest, [{:type, [name, _]}, "!value" <> doc], context, _line, _offset) do
    this_value =
      %ExDoc.FunctionNode{
        arity: 0,
        doc: String.trim(doc),
        doc_line: 113,
        group: "Values",
        id: "#{name}",
        name: String.to_atom(name),
        signature: "#{name}",
        source_path: context.source_path,
        specs: [],
        type: :function
      }
    {[], %{context | docs: [this_value | context.docs]}}
  end

  defp typed_docstring(_rest, [{:type, [name, defn]}, doc], context, _line, _offset) do
    this_type =
      %ExDoc.TypeNode{
        arity: 0,
        doc: doc,
        doc_line: 9,
        id: "#{name}/0",
        name: String.to_atom(name),
        signature: name,
        source_path: context.source_path,
        spec: defn,
        type: :type
      }
    {[], %{context | typespecs: [this_type | context.typespecs]}}
  end

  defp typed_docstring(_rest, [{:fn, [name, defn]}, doc], context, _line, _offset) do
    {:ok, types, _, _, _, _} = head_parser(defn)
    arity = div(Enum.count(types), 2)
    comptime = Enum.any?(types, &match?({:comptime, _}, &1))

    this_fn =
      %ExDoc.FunctionNode{
        annotations: (if comptime, do: ["comptime"], else: []),
        arity: arity,
        doc: doc,
        doc_line: 113,
        group: "Functions",
        id: "#{name}/#{arity}",
        name: String.to_atom(name),
        signature: "#{name}#{defn}",
        source_path: context.source_path,
        specs: [],
        type: :function
      }
    {[], %{context | docs: [this_fn | context.docs]}}
  end

  defp typed_docstring(_rest, [{:err, [group | errors]}, doc], context, _line, _offset) do
    exceptions = Enum.map(errors, fn error ->
      errname = "#{context.id}.#{group}.#{error}"
      %ExDoc.ModuleNode{
        doc: doc,
        doc_line: 1,
        function_groups: [],
        group: "Zig Errors",
        type: :exception,
        id: errname,
        module: String.to_atom(errname),
        source_path: context.source_path,
        title: errname}
    end)

    {[], %{context | typespecs: exceptions ++ context.typespecs}}
  end
end
