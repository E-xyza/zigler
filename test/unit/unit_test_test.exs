defmodule ZiglerTest.UnitTestTest do
  use ExUnit.Case, async: true

  @this_file __ENV__.file

  @tag :one
  test "`use Zigler.Unit` requires `use Zigler`" do
    assert_raise CompileError, fn ->
      @this_file
      |> Path.dirname
      |> Path.join("assets/unit_error/no_zigler_error.exs")
      |> Code.compile_file
    end
  end
end
