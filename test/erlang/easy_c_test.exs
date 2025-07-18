# don't test this unless the system has the cblas libray
# for now we can only guarantee that for linux.
if {:unix, :linux} == :os.type() do
  defmodule ZiglerTest.Erlang.EasyCTest do
    use ZiglerTest.IntegrationCase, async: true

    alias ZiglerTest.Compiler

    @compile {:no_warn_undefined, :erlang_easy_c_test}

    @moduletag :erlang
    @test_file to_charlist(Path.join(__DIR__, "src/erlang_easy_c_test"))

    test "doing it with erlang works" do
      Compiler.compile_erlang(@test_file)

      assert [7.0, 11.0, 15.0] ==
               :erlang_easy_c_test.cblas_daxpy(3, 3.0, [1.0, 2.0, 3.0], 1, [4.0, 5.0, 6.0], 1)
    end
  end
end
