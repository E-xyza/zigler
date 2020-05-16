#defmodule ZiglerTest.Integration.ZigTest.ImportTest do
#  use ExUnit.Case
#
#  use Zigler
#
#  import Zigler.Unit
#
#  @moduletag :zigtest
#
#  # imports module support/passing_tests.exs into zigler.
#  # note that this should be precompiled as a result of being in
#  # test/support directory.
#  zigtest ZiglerTest.ZigTest.ImportTest
#
#  # make sure the existing module recapitulates the code from the
#  # tested module
#
#  test "this module has the the transitive test" do
#    assert function_exported?(__MODULE__, :"transitive zigtest", 0)
#  end
#
#end
