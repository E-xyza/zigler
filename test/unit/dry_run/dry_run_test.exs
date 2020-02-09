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
    Code.ensure_compiled(BasicZeroArity)

    assert [module = %Zigler.Module{}] = BasicZeroArity.__info__(:attributes)[:zigler]

    assert module.dry_run
    assert Enum.any?(module.nifs, &match?(%{name: :zeroarity, arity: 0}, &1))

    assert function_exported?(BasicZeroArity, :zeroarity, 0)
  end
end
