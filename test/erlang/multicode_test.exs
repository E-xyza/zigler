defmodule ZiglerTest.Erlang.MulticodeTest do
  use ZiglerTest.IntegrationCase, async: true
  alias ZiglerTest.Compiler

  @compile {:no_warn_undefined, :erlang_multicode_test}

  @moduletag :erlang
  @test_file to_charlist(Path.join(__DIR__, "src/erlang_multicode_test"))

  test "multicode segments work" do
    Compiler.compile_erlang(@test_file)

    assert 47 == :erlang_multicode_test.part1()
    assert 48 == :erlang_multicode_test.part2()
  end
end
