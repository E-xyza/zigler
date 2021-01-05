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
  <% target = target_struct(@compiler_target, zig_tree) %>

  const for_wasm = true;

  pub fn build(b: *Builder) void {
      const mode = b.standardReleaseOptions();
      const target = b.standardTargetOptions(<%= to_structdef target %>);

      const cflags = [_][]const u8{};
      const lib = b.addSharedLibrary(
          "<%= @module_spec.module %>",
          "<%= @code_file %>",
          .{ .versioned = .{.major = <%= @module_spec.version.major %>,
                            .minor = <%= @module_spec.version.minor %>,
                            .patch = <%= @module_spec.version.patch %>}});

      lib.addSystemIncludeDir("<%= :code.root_dir %>/erts-<%= :erlang.system_info(:version) %>/include");
      <%= unless @module_spec.link_libc do %>
      <%= for dir <- dirs_for(target) do %>
      lib.addSystemIncludeDir("<%= Path.join(zig_tree, dir) %>");
      <% end %>
      <% end %>

      <%= for system_include_dir <- @module_spec.system_include_dirs do %>
      lib.addSystemIncludeDir("<%= system_include_dir %>");
      <% end %>
      <%= for include_dir <- @module_spec.include_dirs do %>
      lib.addIncludeDir("<%= include_dir %>");
      <% end %>

      lib.setBuildMode(mode);
      lib.setTarget(target);
      lib.single_threaded = true;

      <%= if @module_spec.link_libc do %>
      // use libc if it has been asked for
      lib.linkSystemLibrary("c");
      <% end %>

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
  """, [:assigns, :zig_tree])

  def build(target, zig_tree) do
    build_zig_path = Path.join(target.assembly_dir, "build.zig")
    File.write!(build_zig_path, target |> Map.from_struct |> build_zig(zig_tree))
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

  def target_struct(:host, zig_tree) do
    {targets, 0} = zig_tree
    |> Path.join("zig")
    |> System.cmd(["targets"])

    %{"abi" => abi, "cpu" => %{"arch" => arch}, "os" => os} = targets
    |> Jason.decode!
    |> Map.get("native")

    %{abi: abi, arch: arch, os: os}
  end
  def target_struct(_other) do
    "CC"
    |> System.get_env
    |> System.cmd(~w(- -dumpmachine))
    |> elem(0)
    |> String.split("-")
    |> case do
      ["i386"| _] -> %{arch: "i386", os: "linux", abi: "gnu"}
      [arch | _] -> %{arch: arch, os: "linux", abi: "gnu"}
    end
  end

  defp to_structdef(t) do
    ".{.default_target = .{.cpu_arch = .#{t.arch}, .os_tag = .#{t.os}, .abi = .#{t.abi}}}"
  end

  defp dirs_for(target = %{os: "windows"}) do
    ["lib/libc/include/any-windows-any/"] ++ dirs_for_specific(target)
  end
  defp dirs_for(target) do
    ["lib/libc/musl/include/"] ++ dirs_for_specific(target)
  end
  defp dirs_for_specific(%{abi: _, os: os, arch: "x86_64"}) do
    ["lib/libc/include/x86_64-#{os}-musl"]
  end
  defp dirs_for_specific(%{abi: abi, os: os, arch: arch}) do
    ["lib/libc/include/#{arch}-#{os}-#{abi}"]
  end

end
