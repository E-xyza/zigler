defmodule ZiglerTest.Erlang.BasicTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :erlang
  @test_file to_charlist(Path.join(__DIR__, "src/erlang_basic_test"))

  test "doing it with erlang works" do
    {:ok, mod} = :compile.file(@test_file, outdir: :code.lib_dir(:zigler, :ebin))
    Code.ensure_loaded(mod)

    assert 47 == :erlang_basic_test.foo()
    assert 48 == :erlang_basic_test.foo(1)
  end
end
