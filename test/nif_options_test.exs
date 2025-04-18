defmodule ZiglerTest.OptionsTest do
  use ExUnit.Case, async: true

  alias Zig.Nif

  defp make_nif(opts) do
    Nif.new(
      :my_nif,
      %Zig.Module{otp_app: :zigler, module: __MODULE__, file: __ENV__.file, line: __ENV__.line},
      opts
    )
  end

  test "empty is viable" do
    assert %Nif{} = make_nif([])
  end

  describe "export option" do
    test "defaults to true" do
      assert %{export: true} = make_nif([])
    end

    test "accepts boolean" do
      assert %{export: true} = make_nif(export: true)
    end

    test "rejects non-boolean" do
      assert_raise CompileError, "nif option `export` must be a boolean, got: 1", fn ->
        make_nif(export: 1)
      end
    end
  end

  describe "concurrency option" do
    test "defaults to synchronous" do
      assert %{concurrency: Zig.Nif.Synchronous} = make_nif([])
    end

    test "accepts valid options" do
      assert %{concurrency: Zig.Nif.Synchronous} = make_nif(concurrency: :synchronous)
      assert %{concurrency: Zig.Nif.Threaded} = make_nif(concurrency: :threaded)
      assert %{concurrency: Zig.Nif.Yielding} = make_nif(concurrency: :yielding)
      assert %{concurrency: Zig.Nif.DirtyCpu} = make_nif(concurrency: :dirty_cpu)
      assert %{concurrency: Zig.Nif.DirtyIo} = make_nif(concurrency: :dirty_io)
    end

    test "rejects invalid options" do
      assert_raise CompileError,
                   "nif option `concurrency` must be one of `:dirty_cpu`, `:dirty_io`, `:synchronous`, `:threaded`, `:yielding`, got: `:invalid`",
                   fn ->
                     make_nif(concurrency: :invalid)
                   end
    end
  end

  describe "spec option" do
    test "defaults to true" do
      assert %{spec: true} = make_nif([])
    end

    test "is false for :nospec" do
      assert %{spec: false} = make_nif([:nospec])
    end

    test "accepts boolean" do
      assert %{spec: false} = make_nif(spec: false)
    end

    test "rejects non-boolean" do
      assert_raise CompileError, "nif option `spec` must be a boolean, got: 1", fn ->
        make_nif(spec: 1)
      end
    end
  end

  describe "leak check option" do
    test "defaults to false" do
      assert %{leak_check: false} = make_nif([])
    end

    test "is true for :leak_check" do
      assert %{leak_check: true} = make_nif([:leak_check])
    end

    test "accepts boolean" do
      assert %{leak_check: true} = make_nif(leak_check: true)
    end

    test "rejects non-boolean" do
      assert_raise CompileError, "nif option `leak_check` must be a boolean, got: 1", fn ->
        make_nif(leak_check: 1)
      end
    end
  end

  describe "cleanup option" do
    test "defaults to true" do
      assert %{cleanup: true} = make_nif([])
    end

    test "is false for :noclean" do
      assert %{cleanup: false} = make_nif([:noclean])
    end

    test "accepts boolean" do
      assert %{cleanup: false} = make_nif(cleanup: false)
    end

    test "rejects non-boolean" do
      assert_raise CompileError, "nif option `cleanup` must be a boolean, got: 1", fn ->
        make_nif(cleanup: 1)
      end
    end
  end
end
