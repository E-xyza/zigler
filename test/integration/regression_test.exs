defmodule ZiglerTest.RegressionTest do
  defmodule NakedFunction do
    #
    # this tests to make sure you can have a naked function (aka not bound to a
    # "nif:" declaration) with a custom type inside of the regression test.
    #
    use Zigler, app: :zigler

    ~Z"""
    /// nif: noop/0
    fn noop() void {
    }

    const my_int = i64;
    fn helper(val: my_int) i64 {
      return val;
    }
    """
  end
end
