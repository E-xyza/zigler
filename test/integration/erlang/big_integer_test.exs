defmodule ZiglerTest.Erlang.BigIntegerTest do
  use ZiglerTest.IntegrationCase, async: true

  @test_file to_charlist(Path.join(__DIR__, "src/erlang_big_integer_test.erl"))

  require ZiglerTest.ErlangCase
  ErlangCase.initialize()

  test "big integers in erlang work" do
    assert 0xFFFF_FFFF_FFFF_FFFF_FFFF ==
             :erlang_big_integer_test.add_one(0x1_0000_0000_0000_0000_0000)
  end
end
