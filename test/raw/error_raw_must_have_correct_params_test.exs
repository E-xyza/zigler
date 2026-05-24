defmodule ZiglerTest.Raw.ErrorRawMustHaveCorrectParamsTest do
  use ExUnit.Case, async: true
  require ZiglerTest.Compiler

  test "correct params is required" do
    assert_raise CompileError,
                 "test/raw/error_raw_must_have_correct_params.ex:7: nif function `raw` cannot have a value of type beam.env as a parameter",
                 fn ->
                   ZiglerTest.Compiler.compile("error_raw_must_have_correct_params.ex")
                 end
  end
end
