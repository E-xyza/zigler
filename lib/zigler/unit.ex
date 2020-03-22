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

  @doc """
  loads a module that wraps a zigler NIF, consults the corresponding zig code,
  generating the corresponding zig tests.

  Must be called from within a module that has `use ExUnit.Case` and `use Zigler`
  """
  defmacro zigtest(mod) do

    test_zigler = Module.get_attribute(__CALLER__.module, :zigler)

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
      _ ->
        raise CompileError, info ++ [description: "zigtest called on a module that doesn't bind a zig nif"]
    end

    # parse the zigler code to generate the zigcode test and
    # also get the list of nifs.
    ref_zigler.code
    |> IO.iodata_to_binary
    |> Zigler.Parser.Unit.unit_parser(context: %{file: ref_zigler.file})
    |> case do
      {:ok, _code, _, %{tests: []}, _, _} ->
        raise CompileError, info ++ [description: "module #{module} has no zig tests"]
      {:ok, test_code,_, %{tests: tests}, _, _} ->
        new_zigler =  %{test_zigler | code: test_code, nifs: tests}
        Module.put_attribute(__CALLER__.module, :zigler, new_zigler)
        new_zigler
      err ->
        raise CompileError, info ++ [description: "error parsing code in module #{module}: #{inspect err}"]
    end
    |> make_code
  end

  defp make_code(%{module: module, file: file, nifs: nifs}) do
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

