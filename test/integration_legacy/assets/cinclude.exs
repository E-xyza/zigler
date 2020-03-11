defmodule ZiglerTest.Cinclude do
  #
  # tests to make sure we can have working cinclude files.
  #

  use Zigler, otp_app: :zigler

  ~Z"""
  const custom_h = @cImport({
    @cInclude("custom.h");
  });

  /// nif: alloc/0
  fn alloc() void {
    const res = custom_h.enif_alloc(1);
  }
  """

end
