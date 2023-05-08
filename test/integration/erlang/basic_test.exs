defmodule ZiglerTest.Erlang.BasicTest do
  use ZiglerTest.IntegrationCase, async: true

  @test_file to_charlist(Path.join(__DIR__, "src/erlang_basic"))

  require ZiglerTest.ErlangCase
  ErlangCase.initialize()

  test "doing it with erlang works" do
    assert 47 == mod.foo()
    assert 48 == mod.foo(1)
  end
end
