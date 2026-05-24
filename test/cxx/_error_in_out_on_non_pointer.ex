defmodule ZiglerTest.CXX.ErrorInOutOnNonPointer do
  @moduledoc false
  use Zig,
    otp_app: :zigler,
    nifs: [
      takes_int: [params: %{0 => [in_out: true]}]
    ]

  ~Z"""
  pub fn takes_int(value: i32) i32 {
      return value + 1;
  }
  """
end
