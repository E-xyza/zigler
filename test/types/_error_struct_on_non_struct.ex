defmodule ZiglerTest.Types.ErrorStructOnNonStruct do
  @moduledoc false
  use Zig,
    otp_app: :zigler,
    nifs: [
      returns_int: [return: [struct: SomeModule]]
    ]

  ~Z"""
  pub fn returns_int() i32 {
      return 42;
  }
  """
end
