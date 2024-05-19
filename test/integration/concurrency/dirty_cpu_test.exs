defmodule ZiglerTest.Concurrency.DirtyCpuTest do
  use ZiglerTest.IntegrationCase, async: true

  Module.register_attribute(__MODULE__, :zigler_opts, persist: true)
  use Zig, otp_app: :zigler, nifs: [dirty_cpu: [:dirty_cpu]]

  ~Z"""
  pub fn dirty_cpu() void {}
  """

  test "dirty_cpu tagged function" do
    assert :ok = dirty_cpu()
  end

  test "the module file has the correct invocation" do
    [opts] = __MODULE__.__info__(:attributes)[:zigler_opts]

    assert File.read!(opts.module_code_path) =~ ".{ .name = \"dirty_cpu\", .arity = 0, .fptr = dirty_cpu, .flags = 0 }" 
  end
end
