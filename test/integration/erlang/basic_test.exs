defmodule ZiglerTest.Erlang.BasicTest do
  use ZiglerTest.IntegrationCase, async: true

  @test_file to_charlist(Path.join(__DIR__, "src/test_zigler_erlang"))

  test "doing it with erlang works" do
    {:ok, mod} = :compile.file(@test_file, outdir: :code.lib_dir(:zigler, :ebin))
    Code.ensure_loaded(mod)

    assert 47 == mod.foo()
    assert 48 == mod.foo(1)
  end
end
