defmodule Zigler.Unit.Import do
  defstruct [:identifier, :path]
end

defmodule Zigler.Unit.Parser do
  import NimbleParsec

  alias Zigler.Unit

  defp save_test(_rest, ["\"", title, "\"" | _], context, _, _) do
    hash_title = Unit.string_to_hash(title)
    unit = %Unit{
      title: title,
      name: hash_title
    }
    {["pub fn #{hash_title}() !void", unit], context}
  end

  defp make_identifier(_rest, [rest, leading], context, _, _) do
    {[IO.iodata_to_binary([leading, rest])], context}
  end

  defp rebuild_import(_rest, [tail, filepath, identifier], context, _, _) do
    {["pub const #{identifier} = @import(\"#{filepath}\");#{tail}",
       %Unit.Import{identifier: identifier, path: filepath}], context}
  end

  defp rebuild_assert(_rest, assert_line, context, _, _) do
    rebuilt_line = Enum.map(assert_line, fn
      "assert" -> "try assert"
      any -> any
    end)
    {rebuilt_line, context}
  end

  whitespace = ascii_string([?\s, ?\n], min: 1)

  identifier = ascii_char([?a..?z, ?A..?Z, ?_])
  |> optional(ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_], min: 1))
  |> post_traverse(:make_identifier)

  imports = repeat(ascii_char([?s]))
  |> ignore(optional(string("pub") |> concat(whitespace)))
  |> ignore(string("const"))
  |> ignore(whitespace)
  |> concat(identifier)
  |> optional(ignore(whitespace))
  |> ignore(string("="))
  |> optional(ignore(whitespace))
  |> ignore(string("@import("))
  |> optional(ignore(whitespace))
  |> ignore(string("\""))
  |> utf8_string([not: ?"], min: 1)
  |> ignore(string("\""))
  |> optional(ignore(whitespace))
  |> ignore(string(")"))
  |> optional(ignore(whitespace))
  |> ignore(string(";"))
  |> optional(ascii_string([not: ?\n], min: 1))
  |> string("\n")
  |> post_traverse(:rebuild_import)

  test_header =
    repeat(ascii_char([?\s]))
    |> concat(
      string("test")
      |> concat(whitespace)
      |> string("\"")
      |> utf8_string([not: ?"], min: 1)
      |> string("\"")
      |> post_traverse(:save_test))
    |> concat(whitespace)
    |> string("{")
    |> repeat(ascii_char(not: ?\n))
    |> string("\n")

  assert_line =
    optional(ascii_string([?\s], min: 1))
    |> string("assert")
    |> repeat(ascii_char(not: ?\n))
    |> string("\n")
    |> post_traverse(:rebuild_assert)

  if Mix.env == :test do
    defparsec :parse_identifier, identifier
    defparsec :parse_imports, imports
    defparsec :parse_test_header, test_header
  end

  line =
    utf8_string([not: ?\n], min: 1)
    |> string("\n")
    |> reduce({Enum, :join, []})

  empty_line = string("\n")

  by_line =
    repeat(choice([
      imports,
      test_header,
      assert_line,
      line,
      empty_line
    ]))

  defparsec :test_by_line, by_line

  # TODO: test this function to make sure it's outputting reasonable results.
  def get_tests(code, code_file) do
    {:ok, parsed, _, _, _, _} = test_by_line(code)

    {modified_code, tests_and_imports} = Enum.split_with(parsed, fn
      %_{} -> false
      _ -> true
    end)

    # TODO: Keep track of multiply imported filse so we don't double import them.
    tests = tests_and_imports
    |> Enum.flat_map(fn
      u = %Unit{} -> [u]
      %Unit.Import{identifier: id, path: path} ->
        import_file_path = Path.expand(path, Path.dirname(code_file))

        # check to make sure we can proceed.  If not, don't worry,
        # let the zig compiler throw the CompilerError.
        if File.exists?(import_file_path) do
          recursive_get_tests(import_file_path, id)
        else
          []
        end
    end)

    %{code: modified_code, tests: tests}
  end

  def modify_file(content, original_path) do

    {:ok, parsed, _, _, _, _} = test_by_line(content <> "\n")

    modified_content = Enum.reject(parsed, &is_map/1)

    ["""
    const beam = @import("beam.zig");
    const assert = beam.assert;
    // #{original_path} line: 0
    """,
    modified_content]
  end

  @spec recursive_get_tests(Path.t, String.t) :: [Unit.t]
  defp recursive_get_tests(code_path, id) do
    {:ok, parsed, _, _, _, _} = code_path
    |> File.read!
    |> Kernel.<>("\n")
    |> test_by_line

    parsed
    |> Enum.filter(&(match?(%_{}, &1)))
    |> Enum.flat_map(fn
      u = %Unit{} -> [%{u | name: id <> "." <> u.name}]
      %Unit.Import{identifier: new_id, path: path} ->
        import_file_path = Path.expand(path, Path.dirname(code_path))

        # check to make sure we can proceed.  If not, don't worry,
        # let the zig compiler throw the CompilerError.
        if File.exists?(import_file_path) do
          recursive_get_tests(import_file_path, "#{id}.#{new_id}")
        else
          []
        end
    end)
  end

end
