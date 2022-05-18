defmodule Zig.Builder do

  @moduledoc """
  Code for interfacing with `std.build.Builder`, the interface for programmatically invoking
  build code with the `zig build` command.
  """

  require EEx
  require Logger

  build_zig_template = Path.join(__DIR__, "templates/build.zig.eex")
  EEx.function_from_file(:defp, :build_zig, build_zig_template, [:assigns, :zig_tree])

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

  def target_struct(_other, zig_tree) do
    System.get_env
    |> target_struct_from_env(zig_tree)
  end

  def target_struct_from_env(%{
        "TARGET_ABI" => abi,
        "TARGET_ARCH" => arch,
        "TARGET_OS" => os,
        "TARGET_CPU" => cpu
      }, _zig_tree) do
    %{abi: abi, arch: arch, os: os, cpu: cpu}
  end

  def target_struct_from_env(%{"CC" => cc}, _zig_tree) do
    cc
    |> System.cmd(~w(- -dumpmachine))
    |> elem(0)
    |> String.trim
    |> String.split("-")
    |> Enum.reject(&(&1 == "unknown"))
    |> case do
      [arch, os, abi] -> %{arch: arch, os: os, abi: abi}
    end
  end

  def target_struct_from_env(_, zig_tree) do
    # fall back to the default zig identification
    target_struct(:host, zig_tree)
  end

  defp to_structdef(t = %{cpu: cpu}) do
    # NB: this uses zig's duck-typing facilities to only set the cpu_model field when cpu is provided.
    # .explicit field is only available when it's arm; x86 will ignore this extra field.
    ".{.default_target = .{.cpu_arch = .#{t.arch}, .os_tag = .#{t.os}, .abi = .#{t.abi}, .cpu_model = .{ .explicit = &std.Target.arm.cpu.#{cpu}}}}"
  end
  defp to_structdef(t) do
    ".{.default_target = .{.cpu_arch = .#{t.arch}, .os_tag = .#{t.os}, .abi = .#{t.abi}}}"
  end

  defp dirs_for(target = %{os: "windows"}) do
    ["lib/libc/include/any-windows-any/"] ++ dirs_for_specific(target)
  end
  defp dirs_for(target) do
    ["lib/libc/musl/include"] ++ dirs_for_specific(target)
  end

  defp dirs_for_specific(%{abi: _abi, os: os, arch: arch}) do
    ["lib/libc/include/#{arch}-#{os}-musl"]
  end
end
