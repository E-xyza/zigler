defmodule ZiglerTest.Erlang.ErroringTest do
  use ZiglerTest.IntegrationCase, async: true
  alias ZiglerTest.Compiler

  @compile {:no_warn_undefined, :erlang_erroring_test}

  @moduletag :erlang
  @moduletag :erroring

  @test_file to_charlist(Path.join(__DIR__, "src/erlang_erroring_test"))

  test "erroring functions in erlang work" do
    Compiler.compile_erlang(@test_file)

    assert_raise ErlangError, "Erlang error: :some_error", fn ->
      :erlang_erroring_test.errors()
    end
  end
end
