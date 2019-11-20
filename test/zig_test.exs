Code.compile_file("test/assets/zig_test_module.exs")

defmodule ZigTest.ZigTest do
  use ExUnit.Case, async: true
  import Zigler.Unit

  zigtest ZigTest.ZigTestModule
end

#Code.compile_file("test/assets/zig_deep_test_module.exs")
#
#defmodule ZigTest.ZigDeepTest do
#  use ExUnit.Case, async: true
#  import Zigler.Unit
#
#  zigtest ZigTest.ZigDeepTestModule
#end
