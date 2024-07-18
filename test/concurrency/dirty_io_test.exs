defmodule ZiglerTest.Concurrency.DirtyIoTest do
  use ZiglerTest.IntegrationCase, async: true

  Module.register_attribute(__MODULE__, :zigler_opts, persist: true)
  use Zig, otp_app: :zigler, nifs: [dirty_io: [:dirty_io], long_running: [:dirty_io]]

  ~Z"""
  const beam = @import("beam");

  pub fn dirty_io() void {}

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

  test "dirty_io tagged function" do
    assert :ok = dirty_io()
  end

  test "the module file has the correct invocation" do
    [opts] = __MODULE__.__info__(:attributes)[:zigler_opts]

    assert File.read!(opts.module_code_path) =~
             ".{ .name = \"dirty_io\", .arity = 0, .fptr = dirty_io, .flags = e.ERL_NIF_DIRTY_JOB_IO_BOUND }"
  end

  # test "long-running function can yield" do
  #  this = self()
  #  pid = spawn(fn -> long_running(this) end)
  #  assert_receive(:unblock)
  #  Process.exit(pid, :kill)
  #  assert_receive(:killed)
  # end
end
