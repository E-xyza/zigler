# guard against this cblas.h not existing.
if File.exists?("/usr/include/x86_64-linux-gnu/cblas.h") and
     File.exists?("/usr/lib/x86_64-linux-gnu/blas/libblas.so") do
  # prevent CI from running this
  unless System.get_env("RUNNING_CI", nil) do
    defmodule ZiglerTest.Integration.ZigTest.LibraryTest do
      use ExUnit.Case
      use Zig

      import Zig.Unit

      @moduletag :integration
      @moduletag :zigtest

      zigtest(ZiglerTest.ZigTest.Blas)
    end
  end
end
