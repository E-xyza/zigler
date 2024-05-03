defmodule ZiglerTest.Types.OtherTest do
  use ZiglerTest.IntegrationCase, async: true

  describe "beam.env" do
    test "cannot generally be used as a parameter" do
      assert_raise CompileError,
                   "test/integration/types/errors/beam_env_fails.exs:6: functions cannot have beam.env as a parameter",
                   fn ->
                     Code.compile_file("errors/beam_env_fails.exs", __DIR__)
                   end
    end
  end
end
