Code.compile_file("test/assets/zig_test_module.exs")

defmodule ZiglerTest.ZigTest do
  use ExUnit.Case, async: true
  use Zigler.Unit

  zigtest ZiglerTest.ZigTestModule
end

Code.compile_file("test/assets/zig_failing_test_module.exs")

defmodule ZiglerTest.ZigFailingTest do

end

#Code.compile_file("test/assets/zig_deep_test_module.exs")
#
#defmodule ZiglerTest.ZigDeepTest do
#  use ExUnit.Case, async: true
#  import Zigler.Unit
#
#  ZiglerTest ZiglerTest.ZigDeepTestModule
#end
