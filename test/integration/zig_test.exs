defmodule ZiglerTest.ZigTest do
  use ExUnit.Case

  use Zigler

  import Zigler.Unit

  @moduletag :zigtest

  # imports module support/passing_tests.exs into zigler.
  zigtest ZiglerTest.PassingTests

  # make sure the existing module recapitulates the code from the
  # tested module

  test "this module has the code" do
    [zigler] = __MODULE__.__info__(:attributes)[:zigler]
    assert IO.iodata_to_binary(zigler.code)
      =~ "forty_seven()"
  end

  @this_file __ENV__.file

  test "a test can fail, with the correct line number" do
    @this_file
    |> Path.dirname
    |> Path.join("zig_test_fail_shim.exs")
    |> Code.compile_file

    assert {:error, _zig_file, 16} =
      apply(ZiglerTest.FailShim, :"a lie", [])

    assert {:error, _zig_file, 20} =
      apply(ZiglerTest.FailShim, :"a multiline lie", [])

    assert {:error, _zig_file, 27} =
      apply(ZiglerTest.FailShim, :"a truth and a lie", [])
  end

end
