# Nif concurrency strategies

When execution flow enters a Nif, control is fully relinquished from the 
managed environment of the BEAM VM to a context where the BEAM is more or
less unaware of what is going on.

In general the VM cannot tolerate native code running for [longer than
approximately one millisecond](https://www.erlang.org/doc/man/erl_nif.html#lengthy_work).

There are several tools that the BEAM nif system provides for you to 

## Synchronous

The default mode for Nifs to run is *synchronous*.  Only use this mode if
you are confident that your code can run in under 1ms.

## Dirty CPU

`dirty_cpu` mode is usable when your VM has created *Dirty CPU schedulers*.  By
default, the VM creates one dirty CPU scheduler per CPU core available to it.
Nifs tagged as `dirty_cpu` are allowed to run longer than 1 millisecond.

In order to tag a function as `dirty_cpu`, use the `:dirty_cpu` flag in the
options list for the function in the `:nif` call.

### beam.yield in Dirty CPU

The [`beam.yield`](beam.html#yield) function in dirty CPU mode will detect
if the parent process has died and will return `error.processterminated`.

```elixir
defmodule DirtyCpu do
  use ExUnit.Case, async: true
  use Zig, 
    otp_app: :zigler,
    nifs: [long_running: [:dirty_cpu]]

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");
  // this is a dirty_cpu nif.
  pub fn long_running(env: beam.env, pid: beam.pid) !void {
      // following code triggered when process is killed.
      defer {
        // note that the environment of the parent process is dead,
        // so we have to manually create a new environment and send
        // from it.

        const env2 = beam.alloc_env();
        const msg = beam.make(env2, .killed, .{});
        var pid2 = pid;
        _ = e.enif_send(null, &pid2, env2, msg.v);
        beam.free_env(env2);
      }

      while(true) {
          _ = try beam.send(env, pid, .unblock);
          try beam.yield(env);
      }
  }
  """

  test "dirty cpu can be cancelled" do
    this = self()
    dirty_cpu = spawn(fn -> long_running(this) end)
    assert_receive :unblock
    Process.exit(dirty_cpu, :kill)
    assert_receive :killed
  end
end
```

> ### queue limitations {: .warning }
>
> if you consume all of your dirty cpu schedulers with nif calls, the next
> `dirty_cpu` call will block until a scheduler frees up; this could cause
> undesired latency characteristics.

## Dirty IO

It's not recommended to use `dirty_io` unless you're performing IO operations
and blocking using nif events and blocking operations. 

In order to tag a function as `dirty_io`, use the `:dirty_io` flag in the
options list for the function in the `:nif` call.

## Threaded

`threaded` mode is usable when your OS supports spawning threads.  This is
effectively all current platforms supporting the BEAM VM today.  Zigler
will wrap your function code 

In order to tag a function as `threaded`, use the `:threaded` flag in the
options list for the function in the `:nif` call.  Generally, no other
changes must be made to execute a function in threaded mode.

> ### env in Threaded mode {: .warning }
> 
> The `env` variable when you run in threaded mode is *not* a process-bound
> environment.

### beam.yield in Threaded mode

The [`beam.yield`](beam.html#yield) function in dirty CPU mode will detect
if the parent process has died and will return `error.processterminated`.

```elixir
defmodule Threaded do
  use ExUnit.Case, async: true
  use Zig, 
    otp_app: :zigler,
    nifs: [long_running: [:threaded]]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  pub fn long_running(env: beam.env, pid: beam.pid) !void {
      // following code triggered when process is killed.
      defer _ = beam.send(env, pid, .killed) catch {};

      while(true) {
          _ = try beam.send(env, pid, .unblock);
          std.time.sleep(1_000_000);
          try beam.yield(env);
      }
  }
  """

  @tag :threaded
  test "threaded can be cancelled" do
    this = self()
    threaded = spawn(fn -> long_running(this) end)
    #assert_receive :unblock
    Process.sleep(100)
    Process.exit(threaded, :kill)
    assert_receive :killed
    Process.sleep(1000)
  end
end
```

## Yielding

> ### yielding nifs {: .error }
>
> Yielding nifs are not available in this release of Zigler

```elixir
# module
```