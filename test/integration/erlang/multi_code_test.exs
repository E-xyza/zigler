defmodule ZiglerTest.Erlang.MulticodeTest do
  use ZiglerTest.IntegrationCase, async: true

  @test_file to_charlist(Path.join(__DIR__, "src/erlang_multicode_test"))

  test "multicode segments work" do
    {:ok, mod} = :compile.file(@test_file, outdir: :code.lib_dir(:zigler, :ebin))
    Code.ensure_loaded(mod)

    assert 47 == :erlang_multicode_test.part1()
    assert 48 == :erlang_multicode_test.part2()
  end
end
