defmodule ZiglerTest.Cxx.CompileError do
  use Zig, otp_app: :zigler, c: [include_dirs: "include", src: "src2/compile_error.c", headers: [c: "include/compile_error.h"]]

  ~Z"""
  const c = @import("c");
  pub const foo = c.foo;
  """
end
