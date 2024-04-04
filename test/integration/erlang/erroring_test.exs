defmodule ZiglerTest.Erlang.ErroringTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :erlang
  @moduletag :erroring

  @test_file to_charlist(Path.join(__DIR__, "src/erlang_erroring_test"))

  test "erroring functions in erlang work" do
    {:ok, mod} = :compile.file(@test_file, outdir: :code.lib_dir(:zigler, :ebin))
    Code.ensure_loaded(mod)

    assert_raise ErlangError, "Erlang error: :some_error", fn ->
      :erlang_erroring_test.errors()
    end
  end
end
