if System.get_env("DISABLE_TESTS", "false") == "true" do

defmodule ZiglerTest.Raw.ErrorRawMustHaveCorrectParamsTest do
  use ExUnit.Case, async: true
  require ZiglerTest.Compiler

  test "correct params is required" do
    assert_raise CompileError,
                 "test/raw/error_raw_must_have_correct_params.ex:6: nif function `raw` cannot have a value of type beam.env as a parameter",
                 fn ->
                   ZiglerTest.Compiler.compile("error_raw_must_have_correct_params.ex")
                 end
  end
end

end
