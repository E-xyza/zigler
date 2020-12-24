# disabled, for now.

##guard against this cblas.h not existing.
#if File.exists?("/usr/include/x86_64-linux-gnu/cblas.h") do
#
#defmodule ZiglerTest.Integration.ZigTest.LibraryTest do
#  use ExUnit.Case
#  use Zig
#
#  import Zig.Unit
#
#  @moduletag :zigtest
#
#  zigtest ZiglerTest.ZigTest.Blas
#
#end
#
#end
