defmodule ZiglerTest.Erlang.ErroringTest do
  use ZiglerTest.IntegrationCase, async: true

  @test_file to_charlist(Path.join(__DIR__, "src/erlang_erroring_test.erl"))

  require ZiglerTest.ErlangCase
  ErlangCase.initialize()

  test "erroring functions in erlang work" do
    assert_raise ErlangError, "", fn ->
      :erlang_erroring_test.errors()
    end
  end
end
