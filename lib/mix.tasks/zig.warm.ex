defmodule Mix.Tasks.Zig.Warm do
  @shortdoc "Precompiles the translate-c toolchain into the global zig cache"

  @moduledoc """
  precompiles the translate-c toolchain into the global zig cache

      $ mix zig.warm

  Zig 0.17 deprecated the built-in translate-c build step, so zigler translates
  erl_nif.h with the ZSF `translate-c` package, which brings in the `aro` C
  frontend.  Building that costs roughly a minute and a gigabyte of memory, once,
  and the result is shared through zig's global cache.

  Nothing needs to call this: the first nif to compile pays the cost, and zig's
  cache lock means concurrent compilations wait for it rather than duplicating the
  work.  It is useful in CI, where that first compilation would otherwise land
  inside a test and can blow a per-test timeout or, on a small runner, an
  unbounded test fan-out can meet it with several builds in flight at once.
  """

  use Mix.Task

  alias Zig.Command
  alias Zig.TranslateC

  @requirements ["app.config"]

  def run(_args) do
    # build inside the cache directory, so translate_c is reachable as ../translate_c
    # -- zig rejects absolute paths in build.zig.zon.
    _translate_c = TranslateC.stage!()
    staging = Path.join(TranslateC.cache_directory(), "warm")

    File.mkdir_p!(staging)

    erts_include =
      Path.join([:code.root_dir(), "/erts-#{:erlang.system_info(:version)}", "/include"])

    File.write!(Path.join(staging, "build.zig"), build_zig())
    File.write!(Path.join(staging, "build.zig.zon"), build_zig_zon())

    Mix.shell().info("precompiling translate-c (this takes a minute the first time)")

    Command.run_zig(
      [
        "build",
        "-Derts_include=#{erts_include}",
        "-Derl_nif_header=#{Path.join(erts_include, "erl_nif.h")}"
      ],
      cd: staging,
      stderr_to_stdout: true
    )

    Mix.shell().info("translate-c is in the global zig cache")
  end

  defp build_zig_zon do
    """
    .{
      .name = .zigler_warm,
      .version = "0.0.0",
      .fingerprint = 0x516e6589bc520873,
      .paths = .{ "build.zig", "build.zig.zon" },
      .dependencies = .{
        .translate_c = .{ .path = "../translate_c" },
      },
    }
    """
  end

  # the smallest build that still forces translate-c (and therefore aro) to be
  # compiled and cached: translate erl_nif.h and throw the result away.
  defp build_zig do
    """
    const std = @import("std");
    const Translator = @import("translate_c").Translator;

    pub fn build(b: *std.Build) void {
        const target = b.graph.host;
        const optimize: std.lang.Optimize = .debug;

        const erts_include = b.option([]const u8, "erts_include", "ERTS include path") orelse
            @panic("erts_include required");
        const erl_nif_header = b.option([]const u8, "erl_nif_header", "path to erl_nif.h") orelse
            @panic("erl_nif_header required");

        const translate_c_dep = b.dependency("translate_c", .{
            .target = target,
            .optimize = optimize,
        });

        const erl: Translator = .init(translate_c_dep, .{
            .name = "erl",
            .c_source_file = .{ .cwd_relative = erl_nif_header },
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        });
        erl.addSystemIncludePath(.{ .cwd_relative = erts_include });

        // depending on the translated file is enough to run the translation.
        b.getInstallStep().dependOn(&b.addInstallFile(erl.output_file, "erl.zig").step);
    }
    """
  end
end
