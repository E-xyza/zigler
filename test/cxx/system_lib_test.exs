if {:unix, :linux} == :os.type() do
  defmodule ZiglerTest.CXX.SystemLibTest do
    use ZiglerTest.IntegrationCase, async: true

    @moduletag :no_ci

    use Zig, otp_app: :zigler, c: [link_lib: {:system, "blas"}]

    ~Z"""
    const c = @cImport(@cInclude("cblas.h"));

    pub const dasum = c.cblas_dasum;
    """

    test "dasum" do
      assert 6.0 ==
               dasum(3, <<1.0::float-native, 2.0::float-native, 3.0::float-native>>, 1)
    end
  end
end
