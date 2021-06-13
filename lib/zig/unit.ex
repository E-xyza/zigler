defmodule Zig.Unit do

  @moduledoc """
  Hooks your zig code into ExUnit, by converting zig tests into ExUnit tests.

  ### Usage

  #### Example

  Inside your zig code (`dependent.zig`):
  ```
  const beam = @import("beam.zig");
  const assert = beam.assert;

  fn one() i64 {
    return 1;
  }

  test "the one function returns one" {
    assert(one() == 1);
  }
  ```

  Inside your elixir code:

  ```
  defmodule MyZigModule do
    use Zig

    ~Z\"""
    const dependent = @import("dependent.zig");

    /// nif: some_nif_fn/1
    ...
    \"""
  end
  ```

  Inside your test module:

  ```
  defmodule MyZigTest do
    use ExUnit.Case, async: true
    use Zig.Unit

    zigtest MyZigModule
  end
  ```

  ### Scope

  zigtest will run tests from the following sources:
  - any tests inside of a sigil Z or sigil z construction
  - any tests inside `pub` `@import` zig sources.
  - any tests inside `pub usingnamespace` zig sources.
  - recursively discovered `pub` structs.
  """

  @doc false
  def name_to_hash(str) do
    Base.encode16(<<:erlang.phash2(str)::32>>)
  end

  @transfer_params [:otp_app, :file, :libs, :resources, :zig_version,
    :imports, :c_includes, :include_dirs, :version, :system_include_dirs,
    :link_libc]

  # an "assert" assignment that substitutes beam assert for std assert
  @assert_assign "const assert = beam.assert;\n"

  alias Zig.Assembler
  alias Zig.Parser.Unit

  @doc """
  loads a module that wraps a zigler NIF, consults the corresponding zig code,
  generating the corresponding zig tests.

  Must be called from within a module that has `use ExUnit.Case` and `use Zig`
  """
  defmacro zigtest(mod, options \\ []) do
    module = Macro.expand(mod, __CALLER__)
    Code.ensure_loaded(module)

    info = __CALLER__
    |> Map.take([:file, :line])
    |> Map.to_list

    unless function_exported?(module, :__info__, 1) do
      raise CompileError, info ++ [description: "zigtest called on a nonexistent module"]
    end

    ref_zigler = case module.__info__(:attributes)[:zigler] do
      [zigler] -> zigler
      _ -> raise CompileError, info ++ [description: "zigtest called on a module that doesn't bind a zig nif"]
    end

    assembly_dir = Path.join(System.tmp_dir!(),
      ".zigler_compiler/#{Mix.env}/#{__CALLER__.module}")

    # convert the code to substitute functions for tests
    {nifs, code} = ref_zigler.code
    |> IO.iodata_to_binary
    |> Unit.parse(info)

    File.mkdir_p!(assembly_dir)

    # gather all code dependencies.  We'll want to look for all things
    # which are labeled as "pub" and modify those code bits accordingly.
    transitive_nifs = ref_zigler.code
    |> Assembler.parse_code(
      parent_dir: Path.dirname(ref_zigler.file),
      target_dir: assembly_dir,
      pub: true,
      context: [])
    |> Enum.flat_map(fn
      assembly = %{pub: true} ->
        # if it's public, we have to rewrite the code on its
        # way in, and retarget it to the transferred file.
        {new_nifs, rewritten_source} = assembly.source
        |> File.read!
        |> Unit.parse(context: assembly.context)

        # write out the file
        File.write!(assembly.target, rewritten_source)

        new_nifs
      _ -> []
    end)

    zigler = struct(Zig.Module, ref_zigler
    |> Map.take(@transfer_params)
    |> Map.merge(%{
      code: [@assert_assign, code],
      nifs: nifs ++ transitive_nifs,
      module: __CALLER__.module}))
    |> Macro.escape

    in_ex_unit = Enum.any?(Application.started_applications(), fn
      {app, _, _} -> app == :ex_unit
    end)

    tests = make_code(
      __CALLER__.module,
      __CALLER__.file,
      nifs,
      !options[:dry_run] && in_ex_unit)

    # trigger the zigler compiler on this module.
    quote do
      Module.put_attribute(unquote(__CALLER__.module), :zigler, unquote(zigler))
      unquote(tests)
    end
  end

  ####################################################################
  ## ExUnit Boilerplate

  defp make_code(module, file, nifs, true) do
    quote bind_quoted: [module: module, file: file, nifs: Macro.escape(nifs)] do
      # register our tests.
      env = __ENV__
      for {name, test} <- Zig.Unit.__zigtests__(module, nifs) do
        @file file
        test_name = ExUnit.Case.register_test(env, :zigtest, name, [])
        def unquote(test_name)(_), do: unquote(test)
      end
    end
  end
  # for testing purposes only:
  defp make_code(module, _, nifs, false) do
    quote bind_quoted: [module: module, nifs: Macro.escape(nifs)] do
      # register our tests.
      for {name, test} <- Zig.Unit.__zigtests__(module, nifs) do
        test_name = name |> Zig.Unit.name_to_hash |> String.to_atom
        def unquote(test_name)(_), do: unquote(test)
      end
    end
  end

  def __zigtests__(module, test_nifs) do
    Enum.map(test_nifs, &{&1.test, test_content(module, &1)})
  end

  defp test_content(module, nif) do
    quote do
      try do
        apply(unquote(module), unquote(nif.test), [])
        :ok
      rescue
        e in ErlangError ->
          error = [
            message: "Zig test failed",
            doctest: ExUnit.AssertionError.no_value(),
            expr: ExUnit.AssertionError.no_value(),
            left: ExUnit.AssertionError.no_value(),
            right: ExUnit.AssertionError.no_value()
          ]
          reraise ExUnit.AssertionError, error, __STACKTRACE__
      end
    end
  end
end
