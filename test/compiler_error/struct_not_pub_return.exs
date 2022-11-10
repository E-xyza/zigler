defmodule ZiglerTest.CompilerError.StructNotPubReturn do
  use Zig,
    compile: false

  ~Z"""
  const not_pub_struct = struct {
    foo: u8
  };

  pub fn should_fail_return(foo: u8) not_pub_struct {
    return .{.foo = foo};
  }
  """
end
