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

end
