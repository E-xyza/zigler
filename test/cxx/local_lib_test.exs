defmodule ZiglerTest.CXX.LocalLibTest do
  use ZiglerTest.IntegrationCase, async: true

  #    use Zig, otp_app: :zigler, c: [link_lib: {:system, "blas"}]
  #
  #    ~Z"""
  #    const c = @cImport(@cInclude("cblas.h"));
  #
  #    pub const dasum = c.cblas_dasum;
  #    """

  @tag :skip
  test "local lib"
end
