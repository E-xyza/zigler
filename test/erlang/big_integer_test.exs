defmodule ZiglerTest.Erlang.BigIntegerTest do
  use ZiglerTest.IntegrationCase, async: true
  alias ZiglerTest.Compiler

  @compile {:no_warn_undefined, :erlang_big_integer_test}

  @moduletag :erlang

  @test_file to_charlist(Path.join(__DIR__, "src/erlang_big_integer_test"))

  test "big integers in erlang work" do
    Compiler.compile_erlang(@test_file)

    assert 0x1_0000_0000_0000_0000_0000 ==
             :erlang_big_integer_test.add_one(0xFFFF_FFFF_FFFF_FFFF_FFFF)
  end
end
