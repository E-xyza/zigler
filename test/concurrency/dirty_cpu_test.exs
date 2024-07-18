defmodule ZiglerTest.Concurrency.DirtyCpuTest do
  use ZiglerTest.IntegrationCase, async: true

  Module.register_attribute(__MODULE__, :zigler_opts, persist: true)
  use Zig, otp_app: :zigler, nifs: [dirty_cpu: [:dirty_cpu], long_running: [:dirty_cpu]]

  ~Z"""
  const beam = @import("beam");

  pub fn dirty_cpu() void {}

  pub fn long_running(pid: beam.pid) !void {
      // following code triggered when process is killed.
      defer {
        beam.independent_context(.{});
        beam.send(pid, .killed, .{}) catch unreachable;
        beam.free_env(beam.context.env);
      }

      try beam.send(pid, .unblock, .{});

      while(true) {
          try beam.yield();
      }
  }
  """

  test "dirty_cpu tagged function" do
    assert :ok = dirty_cpu()
  end

  test "the module file has the correct invocation" do
    [opts] = __MODULE__.__info__(:attributes)[:zigler_opts]

    assert File.read!(opts.module_code_path) =~
             ".{ .name = \"dirty_cpu\", .arity = 0, .fptr = dirty_cpu, .flags = e.ERL_NIF_DIRTY_JOB_CPU_BOUND }"
  end

  test "long-running function can yield" do
    this = self()
    pid = spawn(fn -> long_running(this) end)
    assert_receive(:unblock)
    Process.exit(pid, :kill)
    assert_receive(:killed)
  end
end
