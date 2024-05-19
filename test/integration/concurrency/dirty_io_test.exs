defmodule ZiglerTest.Concurrency.DirtyIoTest do
  use ZiglerTest.IntegrationCase, async: true

  Module.register_attribute(__MODULE__, :zigler_opts, persist: true)
  use Zig, otp_app: :zigler, nifs: [dirty_io: [:dirty_io]]

  ~Z"""
  pub fn dirty_io() void {}
  """

  test "dirty_io tagged function" do
    assert :ok = dirty_io()
  end

  test "the module file has the correct invocation" do
    [opts] = __MODULE__.__info__(:attributes)[:zigler_opts]

    assert File.read!(opts.module_code_path) =~
             ".{ .name = \"dirty_io\", .arity = 0, .fptr = dirty_io, .flags = 0 }"
  end
end
