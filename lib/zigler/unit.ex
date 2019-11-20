# we should figure out a better way to do this.
defmodule Zigler.Unit do

  alias Zigler.Compiler

  # a littlee bit stolen from doctest
  defmacro zigtest(mod) do

    module = Macro.expand(mod, __CALLER__)

    requirement = quote do require unquote(module) end

    file = __CALLER__.file
    line = __CALLER__.line

    tests =
      quote bind_quoted: [module: module, file: file, line: line] do
        env = __ENV__
        file = ExUnit.DocTest.__file__(module)

        for {name, test} <- Zigler.Unit.__zigtests__(module, file, line) |> IO.inspect(label: "21") do
          @file file
          doc = ExUnit.Case.register_test(env, :zigtest, name, [])
          def unquote(doc)(_), do: unquote(test)
        end
      end

    [requirement, tests]
  end

  def __zigtests__(module, file, line) do
    location = [line: line, file: file]
    [{test_name(module), test_content(module, location)}]
  end

  defp test_name(module) do
    "nif in module #{inspect(module)}"
  end

  @zig_dir_path Path.expand("../../../zig", __ENV__.file)

  defp test_content(module, location) do
    # TODO: dry up some of these things against compiler.
    version = module.__info__(:attributes)[:zig_version]
    zig_tree = Path.join(@zig_dir_path, Compiler.basename(version))
    zig_cmd = Path.join(zig_tree, "zig")
    mod_name = Macro.underscore(module)
    tmp_dir = Path.join("/tmp/.elixir-nifs", mod_name)
    stack = Macro.escape([{module, :__MODULE__, 0, location}])
    quote do
      #case System.cmd(unquote(zig_cmd), ["test", "zig_nif.zig"], cd: unquote(tmp_dir), stderr_to_stdout: true) do
      #  {_, 0} -> :ok
      #  {msg, _} ->
      #    error = [
      #      message: "zigtest failed with message #{msg}",
      #    ]
      #    reraise ExUnit.AssertionError, error, unquote(stack)
      #end
      :ok
    end
  end

end
