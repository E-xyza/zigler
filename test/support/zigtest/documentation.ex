defmodule ZiglerTest.Integration.Documentation do
  @moduledoc false

  # checks which make sure documentation generation is OK.
  use Zig, dry_run: true

  ~Z"""
  /// documentation for foo
  /// nif: foo/0
  fn foo() i32 {
    return 47;
  }
  """
end
