defmodule ZiglerTest.Callbacks.OnLoadRawTest do
  # this is a test of the "raw" on_load function.  This means that the
  # function signature of on_load matches the expectation of erl_nif.h
  #
  # the magic __on_load__ function is also tested here.

  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, callbacks: [on_load: :automatic]

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  var stored_number: u32 = undefined;

  pub fn automatic(env: beam.env, _: ?*?*u32, term: e.ErlNifTerm) c_int {
      stored_number = beam.get(u32, .{ .v = term }, .{ .env = env }) catch unreachable;
      return 0;
  }

  pub fn success() beam.term {
      return beam.make(.{ beam.context.mode, stored_number }, .{});
  }
  """

  defp __on_load__, do: 47

  test "on_load can use automatic mode" do
    assert {:synchronous, 47} = success()
  end

  test "the on_load function is not exported" do
    refute :functions
           |> __MODULE__.__info__()
           |> Keyword.has_key?(:automatic)
  end
end
