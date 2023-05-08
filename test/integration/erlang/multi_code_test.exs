defmodule ZiglerTest.Erlang.MulticodeTest do
  use ZiglerTest.IntegrationCase, async: true

  @test_file to_charlist(Path.join(__DIR__, "src/erlang_multicode_test.erl"))

  require ZiglerTest.ErlangCase
  ErlangCase.initialize()

  test "multicode segments work" do
    assert 47 == :erlang_multicode_test.part1()
    assert 48 == :erlang_multicode_test.part2()
  end
end
