defmodule ZiglerTest.Erlang.ThreadedTest do
  use ZiglerTest.IntegrationCase, async: true

  @test_file to_charlist(Path.join(__DIR__, "src/erlang_threaded_test"))

  @moduletag :threaded

  test "threaded function" # do
  #  {:ok, mod} = :compile.file(@test_file, outdir: :code.lib_dir(:zigler, :ebin))
  #  Code.ensure_loaded(mod)
#
  #  this = self()
  #  assert :ok = :erlang_threaded_test.threaded(true, this)
  #  refute_receive :killed
#
  #  pid = spawn(fn -> :erlang_threaded_test.threaded(false, this) end)
  #  :erlang.garbage_collect(pid)
  #  Process.exit(pid, :kill)
  #  assert_receive :killed, 500
  #end
end
