if System.get_env("DISABLE_TESTS", "false") == "true" do
defmodule ZiglerTest.MarkAsImplTest do
  use ZiglerTest.IntegrationCase, async: true

  import ExUnit.CaptureIO

  test "implementations can be marked" do
    refute capture_io(:stderr, fn ->
             Code.compile_file("_mark_as_impl.exs", __DIR__)
           end) =~ "@impl"
  end
end
end
