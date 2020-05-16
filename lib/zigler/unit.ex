defmodule Zigler.Unit do

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
    use Zigler, otp_app: :my_app

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
    use Zigler.Unit

    zigtest MyZigModule
  end
  ```

  ### Scope

  This module will run tests in all zig code that resides in the same code
  directory as the base module (or overridden directory, if applicable).  Zig
  code in subdirectories will not be subjected to test conversion, so if you
  would like to run a subset of tests using the Zig test facility (and without
  the support of a BEAM VM), you should put them in subdirectories.
  """

  @doc false
  def name_to_hash(str) do
    Base.encode16(<<:erlang.phash2(str)::32>>)
  end

  @transfer_params [:file, :libs, :resources, :zig_version, :imports, :c_includes, :include_dirs, :version]

  @doc """
  loads a module that wraps a zigler NIF, consults the corresponding zig code,
  generating the corresponding zig tests.

  Must be called from within a module that has `use ExUnit.Case` and `use Zigler`
  """
  defmacro zigtest(mod, options \\ [test_dirs: ["./"]]) do
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

    # parse the zigler code to generate the zigcode test and
    # also get the list of nifs.
    ref_code = IO.iodata_to_binary(ref_zigler.code)

    {tests, code} = Zigler.Parser.Unit.parse(ref_code, info)

    __CALLER__.module
    |> Module.get_attribute(:zigler)
    |> struct(nifs: tests, code: code)
    |> struct(Map.take(ref_zigler, @transfer_params))
    |> struct(options)
    |> append_import_tests(code, Path.dirname(ref_zigler.file))
    |> update_zigler_struct(info)
    |> make_test_fns
  end

  defp append_import_tests(zigler, code, dir) do
    import_tests = code
    |> IO.iodata_to_binary
    |> Zigler.Parser.Imports.parse
    |> Enum.flat_map(&recursive_find_imports(dir, &1))

    %{zigler | nifs: import_tests ++ zigler.nifs}
  end

  defp recursive_find_imports(file_dir, {import_struct, file}) do
    full_file_path = Path.join(file_dir, file)
    code = File.read!(full_file_path)

    {tests, _} = code
    |> Zigler.Parser.Unit.parse(file: full_file_path)

    imports = code
    |> Zigler.Parser.Imports.parse
    |> Enum.reject(fn {_, file} -> Path.basename(file) == "beam.zig" end)

    tests
    |> Enum.map(fn nif ->
      %{nif |
        name: String.to_atom("#{import_struct}.#{nif.name}"),
        test: String.to_atom("#{import_struct}: #{nif.test}")}
    end)
  end

  defp update_zigler_struct(zigler, info) do
    if zigler.nifs == [] do
      raise CompileError,
        info ++ [description: "zigtest declared for zigler module #{zigler.module} which has no tests"]
    end
    Module.put_attribute(zigler.module, :zigler, zigler)
    zigler
  end

  defp make_test_fns(%{module: module, file: file, nifs: nifs}) do
    tests = Enum.map(nifs, &(&1.test))
    quote bind_quoted: [module: module, file: file, tests: tests] do
      # register our tests.
      env = __ENV__
      for {name, test_code} <- Zigler.Unit.__zigtests__(module, tests) do
        @file file
        test_name = ExUnit.Case.register_test(env, :zigtest, name, [])
        def unquote(test_name)(_), do: unquote(test_code)
      end
    end
  end

  def __zigtests__(module, tests) do
    Enum.map(tests, fn
      title ->
        {title, test_content(module, title)}
    end)
  end

  defp test_content(module, title) do
    quote do
      case apply(unquote(module), unquote(title), []) do
        :ok -> :ok
        {:error, file, line} ->
          error = [
            message: "Zig test \"#{unquote title}\" (assert on line #{line} of #{file}) failed",
            doctest: ExUnit.AssertionError.no_value(),
            expr: ExUnit.AssertionError.no_value(),
            left: ExUnit.AssertionError.no_value(),
            right: ExUnit.AssertionError.no_value()
          ]
          raise ExUnit.AssertionError, error
      end
    end
  end

end

