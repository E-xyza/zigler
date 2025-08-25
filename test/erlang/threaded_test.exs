defmodule ZiglerTest.Erlang.ThreadedTest do
  use ZiglerTest.IntegrationCase, async: true
  alias ZiglerTest.Compiler

  @compile {:no_warn_undefined, :erlang_threaded_test}

  @test_file to_charlist(Path.join(__DIR__, "src/erlang_threaded_test"))

  @moduletag :erlang
  @moduletag :threaded

  test "threaded function" do
    Compiler.compile_erlang(@test_file)

    this = self()
    assert :ok = :erlang_threaded_test.threaded(true, this)
    refute_receive :killed

    pid = spawn(fn -> :erlang_threaded_test.threaded(false, this) end)
    :erlang.garbage_collect(pid)
    Process.exit(pid, :kill)
    assert_receive :killed, 500
  end
end

