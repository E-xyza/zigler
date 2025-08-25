#is_blas_in_priv =
#  :zigler
#  |> :code.priv_dir()
#  |> Path.join("lib/libblas.so")
#  |> File.exists?()
#
#if {:unix, :linux} == :os.type() and is_blas_in_priv do
#  defmodule ZiglerTest.CXX.PrivLibTest do
#    use ZiglerTest.IntegrationCase, async: true
#
#    use Zig, otp_app: :zigler, c: [link_lib: {:priv, "lib/libblas.so"}]
#
#    ~Z"""
#    const c = @cImport(@cInclude("cblas.h"));
#
#    pub const dasum = c.cblas_dasum;
#    """
#
#    test "dasum" do
#      assert 6.0 ==
#               dasum(3, <<1.0::float-native, 2.0::float-native, 3.0::float-native>>, 1)
#    end
#  end
#end
