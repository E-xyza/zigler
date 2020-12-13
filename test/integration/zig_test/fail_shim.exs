defmodule ZiglerTest.Integration.ZigTest.FailShim do
  use ExUnit.Case
  use Zigler
  import Zigler.Unit

  # imports the failing test codes into our system.
  # this can be found in test/support
  zigtest ZiglerTest.ZigTest.FailingTest
end
