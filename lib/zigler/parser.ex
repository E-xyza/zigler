defmodule Zigler.Parser do
  @moduledoc false

  # all functions that parse zig code

  defstruct [:local, :file, global: []]

  import NimbleParsec

  alias Zigler.Parser.{Nif, Resource, ResourceCleanup}

  @type t :: %__MODULE__{
    local: Nif.t | ResourceCleanup.t | {:doc, iodata},
    file: Path.t,
    global: [Nif.t]
  }

  @type line_info :: {non_neg_integer, non_neg_integer}
  @type parsec_retval :: {[String.t], t}

  @alphanumeric [?a..?z, ?A..?Z, ?0..?9, ?_]
  @number [?0..?9]

  #############################################################################
  ## GENERIC NIMBLE_PARSEC PARSERS

  whitespace = ascii_string([?\s, ?\n], min: 1)
  blankspace = ignore(ascii_string([?\s], min: 1))
  # note that tabs are forbidden by zig.

  identifier =
    ascii_char([?a..?z, ?A..?Z, ?_])
    |> optional(ascii_string(@alphanumeric, min: 1))
    |> reduce({IO, :iodata_to_binary, []})

  namespaced_identifier =
    identifier
    |> repeat(concat(string("."), identifier))
    |> reduce({IO, :iodata_to_binary, []})

  number = ascii_string(@number, min: 1)

  error_decorator = string("!")
  pointer_decorator =
    optional(string("?") |> ignore(optional(whitespace)))
    |> string("*")

  array_decorator = string("[")
    |> ignore(optional(whitespace))
    |> optional(string("*c") |> optional(whitespace))
    |> string("]")

  decorator = choice([
    pointer_decorator,
    array_decorator
  ])

  type =
    optional(error_decorator |> ignore(optional(whitespace)))
    |> repeat(decorator |> ignore(optional(whitespace)))
    |> concat(namespaced_identifier)
    |> reduce({IO, :iodata_to_binary, []})

  #############################################################################
  ## INITIALIZATION
  ##
  ## used to initialize nimble_parsec with a struct instead of a generic map.
  ## should be prepended to most things which are turned into parsecs.  You
  ## can also pass information into a parsec function to preseed the context.

  initialize = post_traverse(empty(), :initializer)

  @spec initializer(String.t, [String.t], t, line_info, non_neg_integer)
    :: parsec_retval

  defp initializer(_, _, context, _, _), do: {[], struct(__MODULE__, context)}

  # test harness

  if Mix.env == :test do
    defparsec :parser_initializer, initialize
  end

  #############################################################################
  ## DOCSTRING PARSING
  ##
  ## certain zigler parameters are encoded in zig's /// docstrings.
  ## this includes definitions for nifs and destructors for resources.

  @nif_options Enum.map(~w(long dirty_io dirty_cpu), &string/1)

  docstring_line =
    optional(blankspace)
    |> ignore(string("///"))
    |> optional(blankspace)
    |> lookahead_not(choice([string("nif:"), string("resource:")]))
    |> optional(utf8_string([not: ?\n], min: 1))
    |> ignore(string("\n"))
    |> post_traverse(:register_docstring_line)

  nif_options = repeat(blankspace |> choice(@nif_options))

  # nif declarations take the form:
  # /// nif: <nif_name>/<arity> [nif_options]
  # where nif_options can be "long", "dirty_io", or "dirty_cpu"
  nif_declaration =
    optional(blankspace)
    |> ignore(string("///"))
    |> optional(blankspace)
    |> ignore(string("nif:"))
    |> concat(blankspace)
    |> concat(identifier)
    |> ignore(string("/"))
    |> concat(number)
    |> optional(nif_options)
    |> optional(blankspace)
    |> ignore(string("\n"))
    |> post_traverse(:register_nif_declaration)

  # resource declarations take the form:
  # /// resource: <resource_type> <definition | cleanup>
  resource_modifier = Enum.map(~w(definition cleanup), &string/1)

  resource_declaration =
    optional(blankspace)
    |> ignore(string("///"))
    |> optional(blankspace)
    |> ignore(string("resource:"))
    |> concat(blankspace)
    |> concat(identifier)
    |> optional(blankspace)
    |> choice(resource_modifier)
    |> ignore(string("\n"))
    |> post_traverse(:register_resource_declaration)

  docstring =
    choice([
      times(docstring_line, min: 1)
      |> optional(choice([nif_declaration, resource_declaration])),
      nif_declaration,
      resource_declaration
    ])

  # test harness

  if Mix.env == :test do
    defparsec :parse_docstring_line,       concat(initialize, docstring_line)
    defparsec :parse_nif_declaration,      concat(initialize, nif_declaration)
    defparsec :parse_resource_declaration, concat(initialize, resource_declaration)
    defparsec :parse_docstring,            concat(initialize, docstring)
  end

  # registrations

  @spec register_docstring_line(String.t, [String.t], t, line_info, non_neg_integer)
    :: parsec_retval

  # empty docstring line.
  defp register_docstring_line(_rest, [], context = %{local: {:doc, doc}}, _, _) do
    {[], %{context | local: {:doc, [doc, ?\n]}}}
  end
  defp register_docstring_line(_rest, [], context = %{local: nil}, _, _) do
    {[], %{context | local: nil}}
  end
  defp register_docstring_line(_rest, [content], context = %{local: {:doc, doc}}, _, _) do
    {[], %{context | local: {:doc, [doc, ?\n | String.trim(content)]}}}
  end
  defp register_docstring_line(_rest, [content], context = %{local: nil}, _, _) do
    {[], %{context | local: {:doc, String.trim(content)}}}
  end
  defp register_docstring_line(_rest, [_content], context, {line, _}, _) do
    raise CompileError,
      file: context.file,
      line: line,
      description: "docstring found in an inappropriate context"
  end

  # register_nif_declaration/5: trampolines into register_nif_declaration/3
  @spec register_nif_declaration(String.t, [String.t], t, line_info, non_neg_integer)
    :: parsec_retval
  defp register_nif_declaration(_rest, content, context, _, _) do
    register_nif_declaration(content, context, [])
  end
  # register_nif_declaration/3
  @spec register_nif_declaration([String.t], t, keyword)
    :: parsec_retval
  defp register_nif_declaration(content, context = %{local: {:doc, doc}}, opts) do
    register_nif_declaration(content, %{context | local: nil}, opts ++ [doc: doc])
  end
  defp register_nif_declaration(["long" | rest], context, opts) do
    register_nif_declaration(rest, context, opts ++ [long: true])
  end
  defp register_nif_declaration(["dirty_io" | rest], context, opts) do
    register_nif_declaration(rest, context, opts ++ [dirty: :io])
  end
  defp register_nif_declaration(["dirty_cpu" | rest], context, opts) do
    register_nif_declaration(rest, context, opts ++ [dirty: :cpu])
  end
  defp register_nif_declaration([arity, name], context, opts) do
    local = struct(Nif,
      name:  String.to_atom(name),
      arity: String.to_integer(arity),
      doc:   opts[:doc],
      opts:  Keyword.take(opts, [:long, :dirty]))
    {[], %{context | local: local}}
  end

  defp register_resource_declaration(_rest, ["definition", name], context, _, _) do
    {[], %{context | local: resource_struct(name, context)}}
  end
  defp register_resource_declaration(_rest, ["cleanup", name], context, _, _) do
    {[], %{context | local: resource_cleanup_struct(name, context)}}
  end

  defp resource_struct(name, context = %{local: {:doc, doc}}) do
    struct(resource_struct(name, %{context | local: nil}), doc: doc)
  end
  defp resource_struct(name, _context) do
    struct(Resource, name: String.to_atom(name))
  end

  defp resource_cleanup_struct(name, context = %{local: {:doc, doc}}) do
    struct(resource_cleanup_struct(name, %{context | local: nil}), doc: doc)
  end
  defp resource_cleanup_struct(name, _context) do
    struct(ResourceCleanup, for: String.to_atom(name))
  end

  #############################################################################
  ## FUNCTION HEADER PARSING

  parameter =
    ignore(
      identifier
      |> optional(whitespace)
      |> string(":")
      |> optional(whitespace))
    |> concat(type)

  parameter_list =
    ignore(optional(whitespace))
    |> optional(parameter)
    |> repeat(
      ignore(
        optional(whitespace)
        |> string(",")
        |> optional(whitespace))
      |> concat(parameter))

  function_header =
    ignore(
      optional(blankspace)
      |> string("fn")
      |> concat(whitespace))
    |> concat(identifier)
    |> post_traverse(:validate_nif_declaration)
    |> ignore(optional(whitespace) |> string("("))
    |> concat(parameter_list)
    |> ignore(string(")") |> optional(whitespace))
    |> post_traverse(:validate_arity)
    |> post_traverse(:validate_params)
    |> concat(type)
    |> post_traverse(:validate_retval)
    |> ignore(
      repeat(ascii_char(not: ?\n))
      |> string("\n"))
    |> post_traverse(:register_function_header)

  resource_definition =
    ignore(
      optional(blankspace)
      |> string("const")
      |> concat(whitespace))
    |> concat(identifier)
    |> post_traverse(:validate_resource)
    |> optional(blankspace)
    |> ignore(
      string("=")
      |> repeat(ascii_char(not: ?\n))
      |> string("\n"))
    |> post_traverse(:register_resource_definition)
  # test harness

  if Mix.env == :test do
    defparsec :parse_parameter, concat(initialize, parameter)
    defparsec :parse_parameter_list, concat(initialize, parameter_list)
    defparsec :parse_function_header, concat(initialize, function_header)
    defparsec :parse_resource_definition, concat(initialize, resource_definition)
  end

  # validations

  @spec validate_nif_declaration(String.t, [String.t], t, line_info, non_neg_integer)
    :: parsec_retval | no_return
  defp validate_nif_declaration(_rest, [nif_name], context = %{local: %Nif{name: name}}, {line, _}, _) do
    unless nif_name == Atom.to_string(name) do
      raise CompileError,
        file: context.file,
        line: line,
        description: "nif declaration name #{name} doesn't match function name #{nif_name}"
    end
    {[], context}
  end
  defp validate_nif_declaration(_, content, context, _, _), do: {content, context}

  # validate_arity/5: trampolines into module.validate_arity/3
  @spec validate_arity(String.t, [String.t], t, line_info, non_neg_integer)
    :: parsec_retval | no_return
  defp validate_arity(_rest, params, context = %{local: %module{}}, {line, _}, _) do
    module.validate_arity(Enum.reverse(params), context, line)
    {params, context}
  end
  defp validate_arity(_rest, content, context, _, _), do: {content, context}

  # validate_params/5 : trampolines to module.validate_params/3
  # ignore parameter validation if we're not in a segment specified by a nif.
  @spec validate_params(String.t, [String.t], t, line_info, non_neg_integer)
    :: parsec_retval | no_return
  defp validate_params(_rest, content, context = %{local: %module{}}, {line, _}, _) do
    module.validate_params(content, context, line)
    {content, context}
  end
  defp validate_params(_, content, context, _, _), do: {content, context}

  # validate_retval/5 : trampolines to module.validate_params/3
  @spec validate_retval(String.t, [String.t], t, line_info, non_neg_integer)
    :: parsec_retval | no_return
  defp validate_retval(_rest, content, context = %{local: %module{}}, {line, _}, _) do
    module.validate_retval(content, context, line)
    {content, context}
  end
  defp validate_retval(_, content, context, _, _), do: {content, context}

  @spec validate_resource(String.t, [String.t], t, line_info, non_neg_integer)
    :: parsec_retval | no_return
  defp validate_resource(_, [name], context = %{local: resource = %Resource{}}, {line, _}, _) do
    unless Atom.to_string(resource.name) == name do
      raise CompileError,
        file: context.file,
        line: line,
        description: "resource declaration #{resource.name} doesn't match succeeding const identifier #{name}"
    end
    {[], context}
  end
  defp validate_resource(_, _, context, _, _), do: {[], context}

  # registrations

  @spec register_function_header(String.t, [String.t], t, line_info, non_neg_integer)
    :: parsec_retval

  defp register_function_header(_, content, context = %{local: nif = %module{}}, _, _) do
    {[], %{module.register_function_header(content, context) | local: nil}}
  end
  defp register_function_header(_, _, context, _, _), do: {[], %{context | local: nil}}

  defp register_resource_definition(_, _, context = %{local: resource = %Resource{}}, _, _) do
    {[], %{Resource.register_resource_definition(context) | local: nil}}
  end
  defp register_resource_definition(_, _, context, _, _), do: {[], context}

  #############################################################################
  ## FULL PARSER

  ignored_line =
    optional(utf8_string([not: ?\n], min: 1))
    |> string("\n")
    |> post_traverse(:clear)

  zig_block =
    initialize
    |> repeat(choice([
      docstring,
      function_header,
      resource_definition,
      ignored_line,
    ]))

  defparsec :parse_zig_block, zig_block

  @spec clear(String.t, [String.t], t, line_info, non_neg_integer) :: parsec_retval

  defp clear(_rest, _content, context, _, _) do
    {[], context}
  end

  #############################################################################
  ## API

  @spec parse(String.t, Zigler.Module.t, non_neg_integer) :: Zigler.Module.t
  def parse(code, old_module, line) do
    case parse_zig_block(code, context: Map.from_struct(old_module)) do
      {:ok, [], "", parser, _, _} ->
        append(old_module, parser, code, line)
      {:error, msg, _context, {ctx_line, _}, _} ->
        raise CompileError,
          file: old_module.file,
          line: ctx_line,
          description: msg
      err ->
        raise CompileError,
          file: old_module.file,
          line: line,
          description: "unknown parsing error #{inspect err}"
    end
  end

  #############################################################################
  ## helpers

  defp append(old_module, %{global: global}, code, line) do
    unless Enum.any?(global, &match?(%Nif{}, &1)) do
      raise CompileError,
        file: old_module.file,
        line: line,
        description: "sigil Z doesn't contain any nifs"
    end

    new_nifs = Enum.filter(global, &match?(%Nif{}, &1))
    new_resources = Enum.filter(global, &match?(%Resource{}, &1))
    %{old_module |
      nifs: old_module.nifs ++ new_nifs,
      resources: old_module.resources ++ new_resources,
      code: [old_module.code | code]}
  end
end
