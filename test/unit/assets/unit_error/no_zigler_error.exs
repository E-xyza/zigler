defmodule ZiglerTest.NoZiglerError do
  use ExUnit.Case
  import Zigler.Unit

  zigtest FooModule
  # ^^ this should fail since we haven't done `use Zigler`
end
