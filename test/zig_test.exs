Code.compile_file("test/assets/zig_test_module.exs")

defmodule ZiglerTest.ZigTest do
  use ExUnit.Case, async: true
  use Zigler.Unit

  zigtest ZiglerTest.ZigTestModule
end

# we've precompiled ZigFailingTestShim, in test/support.
# this is a module that uses Zigler.Unit and ExUnit.Case,
# but is basically hidden from the rest because it's precompiled.

defmodule ZiglerTest.ZigFailingTest do

  use ExUnit.Case, async: true

  test "if we have a failing test, it can be caught" do

    test_name = "a lie"
    |> Zigler.Unit.string_to_hash
    |> String.to_atom

    r = assert_raise ExUnit.AssertionError, fn ->
      apply(ZiglerTest.ZigFailingTestShim, test_name, [nil])
    end

    assert %{message: "Zig test failed"} = r
  end

end

#Code.compile_file("test/assets/zig_deep_test_module.exs")
#
#defmodule ZiglerTest.ZigDeepTest do
#  use ExUnit.Case, async: true
#  import Zigler.Unit
#
#  ZiglerTest ZiglerTest.ZigDeepTestModule
#end
