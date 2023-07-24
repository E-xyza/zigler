-module(erlang_threaded_test).
-compile({parse_transform, zigler}). 
-export([threaded/2]).

-zig_code("
  const std = @import(\"std\");
  const beam = @import(\"beam\");
  pub fn threaded(env: beam.env, should_quit: bool, resp: beam.pid) void {
    var i: u32 = 0;
    while (true) : (i += 1) {
      if (i > 5000 and should_quit) {
        break;
      }

      _ = beam.yield(env) catch {
        _ = beam.send(env, resp, .killed) catch unreachable;
        break;
      };
    }
  }").

-zig_opts([{otp_app, zigler}, {nifs, [{threaded, [threaded]}]}]).