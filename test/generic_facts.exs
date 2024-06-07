defmodule ZiglerTest.GenericFacts do
  Module.register_attribute(__MODULE__, :zigler_module, persist: true)

  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler

  ~Z"""
  pub fn foo() u32 { return 47; }
  """

  test "foo contents" do
    file = __ENV__.file

    assert [
             %{
               nifs: [
                 %{
                   name: :foo,
                   file: ^file,
                   line: 8
                 }
               ]
             }
           ] = __MODULE__.__info__(:attributes)[:zigler_module]
  end
end
