defmodule ZiglerTest.Integration.CompileErrorTest do
  use ExUnit.Case, async: true

  def load(name) do
    __ENV__.file
    |> Path.dirname
    |> Path.join("assets/compile_error/#{name}")
    |> Code.eval_file
  end

  @tag :one
  test "zig compiler errors are bound to the correct line number" do
    file = "undeclared_identifier.exs"
    error = assert_raise CompileError, fn -> load(file) end
    assert error.description =~ "undeclared identifier"
    assert error.line == 7
    assert Path.basename(error.file) == file
  end

end
