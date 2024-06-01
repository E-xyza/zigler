defmodule ZiglerTest.Callbacks.OnLoadAutomaticEnumTest do
  # this is a test of the "automatic" on_load function.  This means that the
  # beam.context.env variable is set, and the term value is set to beam.term.
  # the return value is also allowed to be an enum value
  #
  # the magic __on_load__ function is also tested here.

  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, callbacks: [on_load: :automatic]

  ~Z"""
  const beam = @import("beam");

  const RetVal = enum(i8) { ok = 0, bad = -1 };

  var stored_mode: beam.ContextMode = undefined;
  var stored_number: u32 = undefined;

  pub fn automatic(_: ??*?*u32, term: beam.term) RetVal {
      stored_mode = beam.context.mode;
      stored_number = beam.get(u32, term, .{}) catch unreachable;
      return .ok;
  }

  pub fn success() beam.term {
    return beam.make(.{stored_mode, beam.context.mode, stored_number}, .{});
  }
  """

  defp __on_load__, do: 47

  test "on_load can use automatic mode" do
    assert {:callback, :synchronous, 47} = success()
  end

  test "the on_load function is not exported" do
    refute :functions
           |> __MODULE__.__info__()
           |> Keyword.has_key?(:automatic)
  end
end
