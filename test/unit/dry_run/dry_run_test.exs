defmodule ZiglerTest.DryRunTest do
  use ExUnit.Case, async: true

  @moduletag :dry_run

  defmodule BasicZeroArity do
    use Zigler, dry_run: true

    ~Z"""
    /// nif: zeroarity/0
    fn zeroarity() i64 {
      return 47;
    }
    """
  end

  # performs a dry run of zigler compilation pathway, which is everything
  # except for running the zigler compiler

  test "the zeroarity function exists" do
    assert [module = %Zigler.Module{}] = BasicZeroArity.__info__(:attributes)[:zigler]

    assert module.dry_run
    assert Enum.any?(module.nifs, &match?(%{name: :zeroarity, arity: 0}, &1))

    assert function_exported?(BasicZeroArity, :zeroarity, 0)

    # make sure it crashes if we try to run it
    assert_raise RuntimeError, fn -> BasicZeroArity.zeroarity end
  end

  defmodule BasicFortySeven do
    use Zigler, dry_run: true

    ~Z"""
    /// nif: fortyseven/1
    fn fortyseven(foo: i64) i64 {
      return foo + 47;
    }
    """
  end

  test "the fortyseven function exists" do
    assert [module = %Zigler.Module{}] = BasicFortySeven.__info__(:attributes)[:zigler]

    assert module.dry_run
    assert Enum.any?(module.nifs, &match?(%{name: :fortyseven, arity: 1}, &1))

    assert function_exported?(BasicFortySeven, :fortyseven, 1)

    assert_raise RuntimeError, fn -> BasicFortySeven.fortyseven(2) end
  end

end
