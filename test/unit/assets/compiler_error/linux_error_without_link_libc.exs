defmodule ZiglerTest.CompilerError.LinuxErrorWithoutLinkLibc do
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
