defmodule Zig.Builder do

  @moduledoc """
  Code for interfacing with `std.build.Builder`, the interface for programattically invoking
  build code with the `zig build` command.
  """

  require EEx
  require Logger

  EEx.function_from_string(:defp, :build_zig, ~S"""
  const std = @import("std");
  const Builder = std.build.Builder;

  const for_wasm = true;

  pub fn build(b: *Builder) void {
      const mode = b.standardReleaseOptions();
      const target = b.standardTargetOptions(<%= target_struct(@compiler_target) %>);

      const cflags = [_][]const u8{};
      const lib = b.addSharedLibrary(
          "<%= @module_spec.module %>",
          "<%= @code_file %>",
          .{ .versioned = .{.major = <%= @module_spec.version.major %>,
                            .minor = <%= @module_spec.version.minor %>,
                            .patch = <%= @module_spec.version.patch %>}});

      lib.addSystemIncludeDir("<%= :code.root_dir %>/erts-<%= :erlang.system_info(:version) %>/include");
      <%= for system_include_dir <- @module_spec.system_include_dirs do %>
      lib.addSystemIncludeDir("<%= system_include_dir %>");
      <% end %>
      <%= for include_dir <- @module_spec.include_dirs do %>
      lib.addIncludeDir("<%= include_dir %>");
      <% end %>

      lib.setBuildMode(mode);
      lib.setTarget(target);
      lib.single_threaded = true;

      // link libraries.  Always link libc.
      lib.linkSystemLibrary("c");

      <%= for lib <- @module_spec.libs do %>
      <%= cond do %>
        <% String.ends_with?(lib, ".so") -> %>
      lib.linkSystemLibrary("<%= Path.basename(lib) %>");
        <% String.ends_with?(lib, ".dll") -> %>
      lib.linkSystemLibrary("<%= Path.basename(lib) %>");
        <% String.ends_with?(lib, ".dylib") -> %>
      lib.linkSystemLibrary("<%= Path.basename(lib) %>");
        <% String.ends_with?(lib, ".a") -> %>
      lib.addObjectFile("<%= Path.basename(lib) %>");
        <% true -> %>
          <% raise "invalid library file" %>
      <% end %>
      <% end %>

      // strip_symbols option?
      lib.strip = <%= Mix.env() == :prod %>;

      // future feature
      //
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

  ############################################################################
  ## cross-compilation logic.
  ##
  ## this function primarily exists to support Nerves deployments, though
  ## it is possible to set an arbitrary cross-compilation target using a
  ## setting in your `use Zig` directive.  This selects the architecture
  ## by checking your "CC" environment variable, which is in turn set by
  ## Nerves, then adjusts gcc's machine type to a string which allows zig to
  ## select the appropriate cross-compilation settings and libc.

  def target_struct(:host), do: ".{}"
  def target_struct(_other) do
    "CC"
    |> System.get_env
    |> System.cmd(~w(- -dumpmachine))
    |> elem(0)
    |> String.split("-")
    |> to_structdef
  end

  defp to_structdef("i386") do
    ".{.default_target = .{.cpu_arch = .i386, .os_tag = .linux, .abi = .gnu}}"
  end
  defp to_structdef([arch | _]) do
    ".{.default_target = .{.cpu_arch = .#{arch}, .os_tag = .linux, .abi = .gnueabihf}}"
  end

end
