defmodule ZiglerTest.GenericFacts do
  Module.register_attribute(__MODULE__, :zigler_module, persist: true)

  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler

  ~Z"""
  pub fn foo() u32 {
      return 47;
  }
  """

  test "foo contents" do
    assert [
             %{
               nifs: [
                 %{
                   name: :foo,
                   file: "test/generic_facts_test.exs",
                   line: 8
                 }
               ]
             }
           ] = __MODULE__.__info__(:attributes)[:zigler_module]
  end
end
