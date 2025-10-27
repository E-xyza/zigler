# TODO: doesn't work due to /private/var directory problem.
if :os.type() != {:unix, :darwin} do
  defmodule ZiglerTest.DependencyTest do
    use ZiglerTest.IntegrationCase

    require Logger

    use Zig,
      otp_app: :zigler,
      dependencies: [dep: "dependency"],
      extra_modules: [depmod: {:dep, :mymod}]

    ~Z"""
    const depmod = @import("depmod");

    pub const add_one = depmod.add_one;
    """

    test "package file" do
      assert 48 = add_one(47)
    end
  end
end
