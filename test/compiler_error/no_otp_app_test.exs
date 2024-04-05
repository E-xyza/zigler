defmodule ZiglerTest.CompilerError.NoOtpAppTest do
  use ExUnit.Case, async: true

  describe "when a use Zig doesn't have otp_app" do
    test "it raises a compiler error" do
      assert_raise CompileError,
                   "nofile: (module NoOtpApp) you must supply an `otp_app` option to `use Zig`",
                   fn ->
                     Code.compile_quoted(quote do
                      defmodule NoOtpApp do
                        use Zig
                      end
                    end)
                   end
    end
  end 
end