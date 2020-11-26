defmodule Zigler.Builder do

  require EEx
  require Logger

  EEx.function_from_string(:defp, :build_zig, ~S"""
  const Builder = @import("std").build.Builder;

  const for_wasm = true;

  pub fn build(b: *Builder) void {
      const mode = b.standardReleaseOptions();
      const target = b.standardTargetOptions(.{});

      const cflags = [_][]const u8{};
      const lib = b.addSharedLibrary(
          "<%= @module_spec.module %>",
          "<%= @code_file %>",
          .{ .versioned = .{.major = <%= @module_spec.version.major %>,
                            .minor = <%= @module_spec.version.minor %>,
                            .patch = <%= @module_spec.version.patch %>}});

      lib.addSystemIncludeDir("<%= :code.root_dir %>/erts-<%= :erlang.system_info(:version) %>/include");
      lib.setBuildMode(mode);

      lib.setTarget(target);

      // link libraries
      lib.linkSystemLibrary("c");

      // strip_symbols option?
      // lib.strip = true;

      // c files
      // for (cfiles) |c_file| {
      //     lib.addCSourceFile(c_file, &cflags);
      // }

      lib.install();
  }
  """, [:assigns])

  def build(target) do
    build_zig_path = Path.join(target.assembly_dir, "build.zig")
    File.write!(build_zig_path, target |> Map.from_struct |> build_zig)
    Logger.debug("wrote build.zig to #{build_zig_path}")
  end
end
