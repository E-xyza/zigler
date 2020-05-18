#guard against this cblas.h not existing.
if File.exists?("/usr/include/x86_64-linux-gnu/cblas.h") do

# TODO: UNCOMMENT THIS
#defmodule ZiglerTest.Integration.ZigTest.LibraryTest do
#  use ExUnit.Case
#  use Zigler
#
#  import Zigler.Unit
#
#  @moduletag :zigtest
#
#  zigtest ZiglerTest.ZigTest.Blas
#
#end

end
