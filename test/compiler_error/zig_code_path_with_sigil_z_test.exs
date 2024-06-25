defmodule ZiglerTest.CompilerError.ZigCodePathWithSigilZTest do
  use ExUnit.Case, async: true

  describe "when a use Zig has zig code path and sigil_z" do
    test "it raises a compiler error" do
      assert_raise CompileError,
                   "nofile: (module ZigCodePathWithSigilZ) you may not use ~Z when `:zig_code_path` is specified",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZigCodePathWithSigilZ do
                           use Zig, otp_app: :zigler, zig_code_path: "path/to/foo"

                           ~Z"""
                           """
                         end
                       end
                     )
                   end
    end
  end
end
