defmodule ZiglerTest.Erlang.BigIntegerTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :erlang
  @moduletag :skip

  @test_file to_charlist(Path.join(__DIR__, "src/erlang_big_integer_test"))

  test "big integers in erlang work" do
    {:ok, mod} = :compile.file(@test_file, outdir: :code.lib_dir(:zigler, :ebin))
    Code.ensure_loaded(mod)

    assert 0x1_0000_0000_0000_0000_0000 ==
             :erlang_big_integer_test.add_one(0xFFFF_FFFF_FFFF_FFFF_FFFF)
  end
end
