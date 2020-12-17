defmodule ZiglerTest.NoZiglerError do
  use ExUnit.Case
  import Zig.Unit

  zigtest FooModule
  # ^^ this should fail since we haven't done `use Zig`
end
