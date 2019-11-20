defmodule Zigler.Unit do

  @moduledoc """
  traverses the network of a particular
  """

  defstruct [:title, :name]

  @type t :: %__MODULE__{title: String.t, }

  alias Zigler.Unit.Parser

  def string_to_hash(str) do
    hash = :md5
    |> :crypto.hash(str)
    |> Base.encode16

    "test_" <> hash
  end

  defmacro __using__(_) do
    quote do
      # tests are always going to be released in safe mode.
      @release_mode :safe
      # needs to be persisted so that we can store the version for tests.
      Module.register_attribute(__MODULE__, :zig_version, persist: true)

      @on_load :__load_nifs__

      Module.register_attribute(__MODULE__, :zig_specs, accumulate: true)
      Module.register_attribute(__MODULE__, :zig_code, accumulate: true, persist: true)
      Module.register_attribute(__MODULE__, :zig_imports, accumulate: true)

      @before_compile Zigler.Compiler

      import Zigler.Unit
    end
  end

  # a littlee bit stolen from doctest
  defmacro zigtest(mod) do

    module = Macro.expand(mod, __CALLER__)

    code = module.__info__(:attributes)[:zig_code]
    |> IO.iodata_to_binary
    |> Parser.get_tests

    [zigler_app] = module.__info__(:attributes)[:zigler_app]

    [zig_version] = module.__info__(:attributes)[:zig_version]

    code_spec = Enum.map(code.tests, &{&1.name, {[], "void"}})

    empty_functions = Enum.map(code.tests, &Zigler.empty_function(String.to_atom(&1.name), 0))

    compilation = quote do
      @zigler_app unquote(zigler_app)
      @zig_version unquote(zig_version)

      @zig_code unquote(code.code)
      @zig_specs unquote(code_spec)
      unquote_splicing(empty_functions)
    end

    file = __CALLER__.file

    test_list = Enum.map(code.tests, &{&1.title, &1.name})
    test = quote bind_quoted: [module: __CALLER__.module, tests: test_list, file: file] do
      # register our tests.
      env = __ENV__
      for {name, test} <- Zigler.Unit.__zigtests__(module, tests) do
        @file file
        doc = ExUnit.Case.register_test(env, :zigtest, name, [])
        def unquote(doc)(_), do: unquote(test)
      end
    end
    [compilation, test]
  end

  def __zigtests__(module, tests) do
    Enum.map(tests, fn
      {title, name} -> {title, test_content(module, name)}
    end)
  end

  defp test_content(module, name) do
    atom_name = String.to_atom(name)
    quote do
      try do
        apply(unquote(module), unquote(atom_name), [])
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
