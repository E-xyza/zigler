defmodule ZiglerTest.FailShim do
  use ExUnit.Case
  use Zigler
  import Zigler.Unit
  zigtest ZiglerTest.FailingTest
  # imports the failing test nodes into our system.
end
