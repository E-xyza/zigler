defmodule ZiglerTest.CornerCases.FallbackFunctionsTest do
  use ExUnit.Case, async: true

  defmodule NoFallback do
    use Zig, otp_app: :zigler, callbacks: [:on_load]

    ~Z"""
    const beam = @import("beam");
    pub fn on_load(_: ?*?*anyopaque, _: beam.term) c_int {
        return 42;
    }

    pub fn not_loaded() void { unreachable; }
    """
  end

  test "without fallback raises" do
    assert_raise ErlangError, "Erlang error: \"nif for function not_loaded/0 not bound\"", fn ->
      NoFallback.not_loaded()
    end
  end

  defmodule Erroring do
    use Zig, otp_app: :zigler, callbacks: [:on_load]

    ~Z"""
    const beam = @import("beam");
    pub fn on_load(_: ?*?*anyopaque, _: beam.term) c_int {
        return 42;
    }

    pub fn not_loaded() void { unreachable; }
    """

    def not_loaded, do: :error
  end

  test "with fallback does not raise" do
    assert Erroring.not_loaded() == :error
  end

  defmodule NotErroring do
    use Zig, otp_app: :zigler

    ~Z"""
    pub fn not_loaded() void { }
    """

    def not_loaded, do: :error
  end

  test "with no error works" do
    assert NotErroring.not_loaded() == :ok
  end

  defmodule Marshalled do
    use Zig, otp_app: :zigler, callbacks: [:on_load]

    ~Z"""
    const beam = @import("beam");
    pub fn on_load(_: ?*?*anyopaque, _: beam.term) c_int {
        return 42;
    }

    // forces marshalling by using a very large integer type
    pub fn not_loaded(_: u128) void { unreachable; }
    """

    def not_loaded(_), do: :error
  end

  test "when marshalled" do
    assert Marshalled.not_loaded(1_000_000_000_000_000_000_000) == :error
  end

  defmodule MarshalledNoErroring do
    use Zig, otp_app: :zigler

    ~Z"""
    // forces marshalling by using a very large integer type
    pub fn not_loaded(_: u128) void { }
    """

    def not_loaded(_), do: :error
  end

  test "when marshalled but not erroring" do
    assert MarshalledNoErroring.not_loaded(1_000_000_000_000_000_000_000) == :ok
  end

  defmodule Threaded do
    use Zig, otp_app: :zigler, callbacks: [:on_load], nifs: [not_loaded: [:threaded]]

    ~Z"""
    const beam = @import("beam");
    pub fn on_load(_: ?*?*anyopaque, _: beam.term) c_int {
        return 42;
    }

    pub fn not_loaded() void { unreachable; }
    """

    def not_loaded, do: :error
  end

  test "when threaded" do
    assert Threaded.not_loaded() == :error
  end

  defmodule ThreadedNoErroring do
    use Zig, otp_app: :zigler, nifs: [not_loaded: [:threaded]]

    ~Z"""
    pub fn not_loaded() void { }
    """

    def not_loaded, do: :error
  end

  test "when threaded but not erroring" do
    assert ThreadedNoErroring.not_loaded() == :ok
  end
end
