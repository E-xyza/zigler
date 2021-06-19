defmodule ZiglerTest.DryRunErrorTest do
  use ExUnit.Case, async: true

  @moduletag :dry_run

  defmodule ZeroArityError do
    use Zig, dry_run: true

    ~Z"""
    /// nif: void_error/1
    fn void_error(input: i64) !void {
      if (input != 47) {
        return error.BadInput;
      }
    }
    """
  end

  defmodule NoError do
    use Zig, dry_run: true

    ~Z"""
    /// nif: void/0
    fn void() void {}
    """
  end

  @zig_error_module __MODULE__.ZeroArityError.ZigError

  test "when you have an erroring function it creates the ZigError module" do
    assert function_exported?(@zig_error_module, :module_info, 1)
  end

  test "the error module is an exception module" do
    assert is_exception(struct(@zig_error_module))
  end

  test "without an error no module is generated" do
    refute function_exported?(__MODULE__.NoError.ZigError, :module_info, 1)
  end

  # 0.8.0, linux, only
  test "this module links libc"
end
