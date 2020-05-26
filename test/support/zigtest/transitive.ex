defmodule ZiglerTest.ZigTest.Transitive do

  @moduledoc false

  use Zigler

  ~Z"""
  pub const trans = @import("imported.zig");
  pub usingnamespace @import("namespaced.zig");
  const nonpub = @import("nonpublic.zig");

  /// nif: foo/0
  fn foo() i32 {
    return nonpub.foo();
  }
  """
end
