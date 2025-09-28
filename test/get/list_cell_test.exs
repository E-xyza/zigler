defmodule ZiglerTest.Get.ListCellTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");

  pub fn add_improper_list(passed: beam.term) !u32 {
      const head, const tail = try beam.get_list_cell(passed, .{});
      return try beam.get(u32, head, .{}) + try beam.get(u32, tail, .{});
  }
  """

  test "get_list_cell" do
    assert 47 = add_improper_list([40 | 7])
  end

  test "raises on other than list" do
    Enum.each(
      [0, 0.0, :foo, "foo", %{}, {}, make_ref(), self()],
      fn
        term ->
          assert_raise ArgumentError, fn ->
            add_improper_list(term)
          end
      end
    )
  end
end
