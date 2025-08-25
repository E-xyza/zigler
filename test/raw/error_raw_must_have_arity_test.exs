if System.get_env("DISABLE_TESTS", "false") == "true" do

defmodule ZiglerTest.Raw.ErrorRawMustHaveArityTest do
  use ExUnit.Case, async: true
  require ZiglerTest.Compiler

  test "arity is required" do
    assert_raise CompileError,
                 "test/raw/error_raw_must_have_arity.ex:6: the raw function ZiglerTest.Raw.ErrorRawMustHaveArity.raw/? must have arities specified in zigler parameters",
                 fn ->
                   ZiglerTest.Compiler.compile("error_raw_must_have_arity.ex")
                 end
  end
end

end
