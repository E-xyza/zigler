defmodule ZiglerTest.Integration.ZigTest do
  use ExUnit.Case, async: true

  use Zig
  import Zig.Unit

  @moduletag :zigtest

  # imports module support/passing_tests.ex into zigler.
  # note that this should be precompiled as a result of being in
  # test/support directory.

  zigtest ZiglerTest.ZigTest.PassingTests

  # make sure the existing module recapitulates the code from the
  # tested module

  test "this module has the code" do
    [zigler] = __MODULE__.__info__(:attributes)[:zigler]
    assert IO.iodata_to_binary(zigler.code)
      =~ "forty_seven()"
  end

  alias ZiglerTest.Integration.ZigTest.FailShim
  test "tests can fail", context do
    __DIR__
    |> Path.join("fail_shim.exs")
    |> Code.compile_file

    assert_raise ExUnit.AssertionError, fn ->
      apply(FailShim, :"zigtest a lie", [context])
    end

    assert_raise ExUnit.AssertionError, fn ->
      apply(FailShim, :"zigtest a multiline lie", [context])
    end

    assert_raise ExUnit.AssertionError, fn ->
      apply(FailShim, :"zigtest a truth and a lie", [context])
    end

    assert_raise FailShim.ZigError, fn ->
      apply(FailShim, :"zigtest an unrelated error", [context])
    end
  end

end
