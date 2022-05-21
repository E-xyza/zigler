defmodule ZiglerTest.Integration.ZigTest.FailShim do
  use ExUnit.Case
  use Zig
  import Zig.Unit

  @moduletag :integration
  @moduletag :error_return

  # imports the failing test codes into our system.
  # this can be found in test/support

  zigtest(ZiglerTest.ZigTest.FailingTest)
end
