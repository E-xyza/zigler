defmodule ZiglerTest.Integration.Strategies.CAndCppSourcesTest do
  use ExUnit.Case, async: true

  @moduletag :integration

  use Zig,     link_libc: true,
    link_libcpp: true,
    include: ["test_c_cpp.h"],
    sources: [
      "_test_c_source.c",
      {"_test_cpp_source.cpp", ["-std=c++17"]}
    ]

  ~Z"""
  const c = @cImport({
    @cInclude("test_c_cpp.h");
  });

  /// nif: c_function/0
  fn c_function() c_int {
    return c.c_function();
  }

  /// nif: cpp_function/0
  fn cpp_function() c_int {
    return c.cpp_function();
  }
  """

  test "elixir persistent memory works" do
    assert c_function() == 47
    assert cpp_function() == 47
  end
end
