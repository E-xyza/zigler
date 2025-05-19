defmodule ZiglerTest.Cxx.CompileError do
  use Zig, otp_app: :zigler, c: [include_dirs: "include", src: "src2/compile_error.c"]

  ~Z"""
  const c = @cImport(@cInclude("compile_error.h"));
  pub const foo = c.foo;
  """
end