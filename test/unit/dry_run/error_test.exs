defmodule ZiglerTest.DryRunErrorTest do
  use ExUnit.Case, async: true

  @moduletag :dry_run

  defmodule ZeroArityError do
    use Zig, link_libc: true

    ~Z"""
    /// nif: void_error/1
    fn void_error(input: i64) !void {
      if (input != 47) {
        return error.BadInput;
      }
    }
    """
  end

  defmodule NoError do
    use Zig

    ~Z"""
    /// nif: void/0
    fn void() void {}
    """
  end

  @zig_error_module __MODULE__.ZeroArityError.ZigError

  test "when you have an erroring function it creates the ZigError module" do
    assert function_exported?(@zig_error_module, :module_info, 1)
  end

  test "the error module is an exception module" do
    assert is_exception(struct(@zig_error_module))
  end

  test "without an error no module is generated" do
    refute function_exported?(__MODULE__.NoError.ZigError, :module_info, 1)
  end

  if match?({:unix, :linux}, :os.type()) do
    describe "in linux" do
      test "it's a compiler error if you seek stacktraces don't link_libc" do
        assert_raise CompileError, fn ->
          __DIR__
          |> Path.join("../assets/compiler_error/linux_error_without_link_libc.exs")
          |> Path.expand()
          |> Code.compile_file()
        end
      end

      @tag :skip
      test "it's not a compiler error if stacktrace info is stripped"
    end
  end
end
