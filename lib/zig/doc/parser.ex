defmodule Zig.Doc.Parser do
  @moduledoc """
  handles parsing zig code for the purposes of documentation
  """

  def docs_from_dir(dir, config) do
    dir
    |> File.ls!()
    # only read .zig files
    |> Enum.filter(&Regex.match?(~r/\.zig$/, &1))
    |> Enum.flat_map(&moduledocs_from_file(dir, &1, config))
  end

  defp interpolate_source_url(pattern, path, line) do
    pattern
    |> String.replace(~r/\%\{path\}/, path)
    |> String.replace(~r/\%\{line\}/, "#{line}")
  end

  defp interpolate_line(line) do
    # hacky retrieval of pattern and path values.
    patt = Process.get(:"$url_patt")
    path = Process.get(:"$rel_path")
    interpolate_source_url(patt, path, line)
  end

  def moduledocs_from_file(dir, file, config) do
    file_path = Path.join(dir, file)
    base = Path.basename(file_path, ".zig")
    rel_path = Path.relative_to(file_path, File.cwd!())
    source_url_pattern = config.source_url_pattern

    init_mod = %ExDoc.ModuleNode{
      doc_line: 1,
      docs_groups: ["Functions", "Values"],
      group: "Zig Structs",
      type: :module,
      id: base,
      module: String.to_atom(base),
      source_path: rel_path,
      source_url: interpolate_source_url(source_url_pattern, rel_path, 1),
      title: base
    }

    # a hacky way of sneaking information into the nimble_parsec
    # parser.
    Process.put(:"$init_mod", init_mod)
    Process.put(:"$rel_path", rel_path)
    Process.put(:"$url_patt", source_url_pattern)

    {:ok, _, _, parsed_module, _, _} = file_parser(File.read!(file_path))

    # split out the error modules from the parsed module.
    {types, exceptions} =
      Enum.split_with(
        parsed_module.typespecs,
        &match?(%ExDoc.TypeNode{}, &1)
      )

    [
      %{
        parsed_module
        | docs: Enum.reverse(parsed_module.docs),
          doc: doc_ast(parsed_module),
          typespecs: Enum.reverse(types)
      }
      | exceptions
    ]
  end

  import NimbleParsec

  whitespace = ascii_string([?\s, ?\n], min: 1)

  docstring_line =
    optional(whitespace)
    |> choice([string("///"), string("//!")])
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
      |> concat(whitespace)
    )
    |> concat(identifier)
    |> ignore(
      optional(whitespace)
      |> string("=")
      |> optional(whitespace)
    )
    |> lookahead_not(string("error"))
    |> concat(ascii_string([not: ?;], min: 1))
    |> ignore(string(";"))
    |> ignore(ascii_string([not: ?\n], min: 1))
    |> ignore(string("\n"))
    |> tag(:type)

  val_head =
    optional(whitespace)
    |> ignore(
      string("pub")
      |> concat(whitespace)
      |> string("var")
      |> concat(whitespace)
    )
    |> concat(identifier)
    |> ignore(
      optional(whitespace)
      |> string("=")
      |> optional(whitespace)
    )
    |> ignore(ascii_string([not: ?\n], min: 1))
    |> ignore(string("\n"))
    |> tag(:val)

  error_head =
    optional(whitespace)
    |> ignore(
      string("pub")
      |> concat(whitespace)
      |> string("const")
      |> concat(whitespace)
    )
    |> concat(identifier)
    |> ignore(
      optional(whitespace)
      |> string("=")
      |> optional(whitespace)
      |> string("error")
      |> optional(whitespace)
      |> string("{")
    )
    |> repeat(
      ignore(optional(whitespace))
      |> optional(repeat(docstring) |> tag(:doc) |> ignore(optional(whitespace)))
      |> concat(identifier)
      |> ignore(optional(whitespace |> string(",")))
    )
    |> ignore(
      optional(whitespace)
      |> string("}")
    )
    |> tag(:err)

  if Mix.env() == :test do
    defparsec(:error_head, error_head)
  end

  fn_head =
    optional(whitespace)
    |> ignore(
      string("pub")
      |> concat(whitespace)
      |> string("fn")
      |> concat(whitespace)
    )
    |> concat(identifier)
    |> concat(ascii_string([not: ?{], min: 1))
    |> ignore(string("{"))
    |> tag(:fn)

  typed_docstring =
    docstring
    |> choice([type_head, error_head, val_head, fn_head])
    |> post_traverse(:typed_docstring)

  file_parser =
    empty()
    |> post_traverse(:initialize)
    |> optional(
      empty()
      |> concat(docstring)
      |> post_traverse(:moduledoc)
    )
    |> repeat(
      choice([
        typed_docstring,
        line
      ])
    )

  defparsec(:file_parser, file_parser)

  defp trim_slashes("//! " <> rest), do: rest
  defp trim_slashes("//!" <> rest), do: rest
  defp trim_slashes("/// " <> rest), do: rest
  defp trim_slashes("///" <> rest), do: rest

  defp strip_docline(line) do
    line
    |> Enum.map(fn
      str = "///" <> _ -> trim_slashes(str)
      any -> any |> String.trim_leading() |> trim_slashes
    end)
    |> Enum.join()
  end

  defp initialize(_rest, content, _context, _line, _offset) do
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
      |> string("(")
    )
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
        |> optional(whitespace)
      )
      |> ascii_string([not: ?,, not: ?\n, not: ?)], min: 1)
      |> ignore(optional(string(",")))
    )
    |> ignore(string(")"))

  defparsec(:head_parser, head_parser)

  # unsure of why this hack is necessary.
  defp typed_docstring(str, [{type, ["\n" | rest1]}, rest2], context, line, offset)
       when is_atom(type) do
    typed_docstring(str, [{type, rest1}, rest2], context, line, offset)
  end

  defp typed_docstring(_rest, [{:type, [name, _]}, "!value" <> doc], context, {line, _}, _offset) do
    this_value = %ExDoc.FunctionNode{
      arity: 0,
      doc: doc_ast(String.trim(doc), context.source_path, line),
      doc_line: line,
      group: "Values",
      id: "#{name}",
      name: String.to_atom(name),
      signature: "#{name}",
      source_path: context.source_path,
      specs: [],
      source_url: interpolate_line(line),
      type: :function
    }

    {[], %{context | docs: [this_value | context.docs]}}
  end

  defp typed_docstring(_rest, [{:type, [name, defn]}, doc], context, {line, _}, _offset) do
    this_type = %ExDoc.TypeNode{
      arity: 0,
      doc: doc_ast(doc, context.source_path, line),
      doc_line: line,
      id: "#{name}/0",
      name: String.to_atom(name),
      signature: name,
      source_path: context.source_path,
      source_url: interpolate_line(line),
      spec: defn,
      type: :type
    }

    {[], %{context | typespecs: [this_type | context.typespecs]}}
  end

  defp typed_docstring(_rest, [{:val, [name | _]}, doc], context, {line, _}, _offset) do
    this_value = %ExDoc.FunctionNode{
      annotations: "mutable",
      arity: 0,
      doc: doc_ast(doc, context.source_path, line),
      doc_line: line,
      group: "Values",
      id: "#{name}",
      name: String.to_atom(name),
      signature: "#{name}",
      source_path: context.source_path,
      source_url: interpolate_line(line),
      specs: [],
      type: :function
    }

    {[], %{context | docs: [this_value | context.docs]}}
  end

  defp typed_docstring(rest, [{:fn, [" " <> _, name, defn]}, doc], context, meta, offset) do
    typed_docstring(rest, [{:fn, [name, defn]}, doc], context, meta, offset)
  end

  defp typed_docstring(_rest, [{:fn, [name, defn]}, doc], context, {line, _}, _offset) do
    {:ok, types, _, _, _, _} = head_parser(defn)
    arity = div(Enum.count(types), 2)
    comptime = Enum.any?(types, &match?({:comptime, _}, &1))

    this_fn = %ExDoc.FunctionNode{
      annotations: if(comptime, do: ["comptime"], else: []),
      arity: arity,
      doc: doc_ast(doc, context.source_path, line),
      doc_line: line,
      group: "Functions",
      id: "#{name}/#{arity}",
      name: String.to_atom(name),
      signature: "#{name}#{defn}",
      source_path: context.source_path,
      source_url: interpolate_line(line),
      specs: [],
      type: :function
    }

    {[], %{context | docs: [this_fn | context.docs]}}
  end

  defp typed_docstring(_rest, [{:err, [group | errors]}, _doc], context, {line, _}, _offset) do
    exceptions = exceptions_from(errors, group, context, line)
    {[], %{context | typespecs: exceptions ++ context.typespecs}}
  end

  defp exceptions_from([], _, _, _), do: []

  defp exceptions_from([{:doc, doc}, error | rest], group, context, line) do
    errname = "#{context.id}.#{group}.#{error}"

    [
      %ExDoc.ModuleNode{
        doc: doc_ast(IO.iodata_to_binary(doc), context.source_path, 1),
        doc_line: 1,
        docs_groups: [],
        group: "Zig Errors",
        type: :exception,
        id: errname,
        module: String.to_atom(errname),
        source_path: context.source_path,
        source_url: interpolate_line(line),
        title: errname
      }
      | exceptions_from(rest, group, context, line)
    ]
  end

  defp exceptions_from([error | rest], group, context, line) do
    errname = "#{context.id}.#{group}.#{error}"

    [
      %ExDoc.ModuleNode{
        doc_line: 1,
        docs_groups: [],
        group: "Zig Errors",
        type: :exception,
        id: errname,
        module: String.to_atom(errname),
        source_path: context.source_path,
        source_url: interpolate_line(line),
        title: errname
      }
      | exceptions_from(rest, group, context, line)
    ]
  end

  defp doc_ast(parsed), do: doc_ast(parsed.doc, parsed.source_path, parsed.doc_line)

  defp doc_ast(markdown, path, line) do
    alias ExDoc.Markdown
    Markdown.to_ast(markdown, file: path, line: line)
  end
end
