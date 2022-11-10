defmodule ZiglerTest.CompilerError.StructNotPubParam do
  use Zig,
    compile: false

  ~Z"""
  const not_pub_struct = struct {
    foo: u8
  };

  pub fn should_fail_param(foo: not_pub_struct) u8 {
    return foo.foo;
  }
  """
end
