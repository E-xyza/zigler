Code.compile_file("test/support/zig_test_module.exs")

defmodule ZigTest.ZigTest do
  use ExUnit.Case, async: true
  import Zigler.Unit

  zigtest ZigTest.ZigTestModule
end
