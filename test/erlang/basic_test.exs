defmodule ZiglerTest.Erlang.BasicTest do
  use ZiglerTest.IntegrationCase, async: true
  alias ZiglerTest.Compiler

  @compile {:no_warn_undefined, :erlang_basic_test}

  @moduletag :erlang
  @test_file to_charlist(Path.join(__DIR__, "src/erlang_basic_test"))

  test "doing it with erlang works" do
    Compiler.compile_erlang(@test_file)

    assert 47 == :erlang_basic_test.foo()
    assert 48 == :erlang_basic_test.foo(1)
  end
end
