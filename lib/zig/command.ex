defmodule Zig.Command do
  @moduledoc false

  # contains all parts of the Zig library involved in calling the
  # zig compiler toolchain, especially with regards to the `zig` command, except
  # for assembling the build.zig file, which is performed by the
  # `Zig.Builder` module.

  alias Zig.Builder
  alias Zig.Target

  require Logger
  require Zig

  #############################################################################
  ## API

  def run_zig(command, opts) do
    args = String.split(command)

    base_opts = Keyword.take(opts, [:cd, :stderr_to_stdout])
    zig_cmd = executable_path()
    Logger.debug("running command: #{zig_cmd} #{command}")

    case System.cmd(zig_cmd, args, base_opts) do
      {result, 0} ->
        result

      {error, code} ->
        raise Zig.CompileError, command: command, code: code, error: error
    end
  end

  def run_sema!(module) do
    # nerves will put in a `CC` command that we need to bypass because it misidentifies
    # libc locations for statically linking it.
    System.delete_env("CC")

    staging_dir = Path.dirname(module.module_code_path)

    run_zig("build -Dzigler-mode=sema", cd: staging_dir, stderr_to_stdout: true)

    attempt_json(staging_dir, 5)
  end

  # Documentation-specific semantic analysis for zig_doc compatibility
  # Uses sema_doc.zig which provides a simpler analysis without full build context
  def run_sema_doc!(file) do
    priv_dir = :code.priv_dir(:zigler)
    sema_file = Path.join(priv_dir, "beam/sema_doc.zig")
    erl_nif_file = Path.join(priv_dir, "beam/stub_erl_nif.zig")
    # Ensure the file path is absolute
    abs_file = Path.expand(file)

    System.delete_env("CC")

    # Create a temporary directory for the doc build
    tmp_dir = Path.join(System.tmp_dir!(), "zigler_doc_#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(tmp_dir)

    # Create build.zig that imports the files we need
    build_zig = """
    const std = @import("std");

    pub fn build(b: *std.Build) void {
        const target = b.standardTargetOptions(.{});
        const optimize = b.standardOptimizeOption(.{});

        const exe = b.addExecutable(.{
            .name = "sema_doc",
            .root_module = b.createModule(.{
                .root_source_file = .{ .cwd_relative = "#{sema_file}" },
                .target = target,
                .optimize = optimize,
            }),
        });

        const erl_nif = b.addModule("erl_nif", .{ .root_source_file = .{ .cwd_relative = "#{erl_nif_file}" } });
        const analyte = b.addModule("analyte", .{ .root_source_file = .{ .cwd_relative = "#{abs_file}" } });

        // analyte (beam.zig) imports erl_nif, so we need to add it to analyte's imports
        analyte.addImport("erl_nif", erl_nif);

        exe.root_module.addImport("erl_nif", erl_nif);
        exe.root_module.addImport("analyte", analyte);

        b.installArtifact(exe);

        const run_cmd = b.addRunArtifact(exe);
        const run_step = b.step("run", "Run sema_doc");
        run_step.dependOn(&run_cmd.step);
    }
    """

    build_zig_path = Path.join(tmp_dir, "build.zig")
    File.write!(build_zig_path, build_zig)

    try do
      # Run zig build to execute sema_doc
      run_zig("build run", cd: tmp_dir, stderr_to_stdout: true)
    after
      File.rm_rf!(tmp_dir)
    end
  end

  # this function needs to be repeated on the windows platform because it sometimes fails with
  # no text.
  defp attempt_json(_, 0) do
    raise Zig.CompileError, command: "sema"
  end

  defp attempt_json(staging_dir, n) do
    staging_dir
    |> Path.join("zig-out/bin/sema")
    |> System.cmd(["--json"])
    |> case do
      {"", 0} ->
        Process.sleep(100)
        attempt_json(staging_dir, n - 1)

      {res, 0} ->
        res

      {error, code} ->
        raise Zig.CompileError, command: "sema", code: code, error: error
    end
  end

  def fmt(file) do
    if System.get_env("ZIG_FMT", "true") != "false" do
      run_zig("fmt #{file}", [])
    end
  end

  def compile!(module = %{precompiled: nil}) do
    staging_directory = Builder.staging_directory(module.module)
    precompiler_settings = precompile_meta()

    so_dir =
      if precompiler_settings do
        Path.dirname(module.file)
      else
        :code.priv_dir(module.otp_app)
      end

    lib_dir = Path.join(so_dir, "lib")
    dst_lib_path = Path.join(lib_dir, dst_lib_name(module))

    target = precompile_name("-Dtarget=")

    run_zig("build #{target} --prefix #{so_dir}",
      cd: staging_directory,
      stderr_to_stdout: true
    )

    Logger.debug("compiled lib at: #{so_dir}")

    src_lib_path =
      if windows?() do
        # windows dlls wind up in the bin directory instead of the lib directory.
        so_dir
        |> Path.join("bin")
        |> Path.join(src_lib_name(module.module))
      else
        Path.join(lib_dir, src_lib_name(module.module))
      end

    # on Windows, not deleting this causes a file permissions error.
    # on MacOS, we must delete the old library because otherwise library
    # integrity checker will kill the process
    #
    # as this is required by two different build systems, we might as well
    # remove on all.
    File.rm(dst_lib_path)

    if src_lib_path != dst_lib_path do
      File.cp!(src_lib_path, dst_lib_path)
    end

    precompile_callback(dst_lib_path)

    Logger.debug("moved library to #{dst_lib_path}")

    module
  rescue
    e in Zig.CompileError ->
      reraise Zig.CompileError.resolve(e, module), __STACKTRACE__
  end

  def compile!(module) do
    # `precompiled` contains the path to a precompiled library.
    Logger.debug(
      "skipping compile step for precompiled module #{module.module}, at #{module.precompiled}"
    )

    so_dir = :code.priv_dir(module.otp_app)
    lib_dir = Path.join(so_dir, "lib")

    dst_lib_path = Path.join(lib_dir, dst_lib_name(module))
    Logger.debug("destination library path: #{dst_lib_path}")

    force_reload = System.get_env("ZIGLER_PRECOMPILED_FORCE_RELOAD", "false") == "true"
    lib_missing = not File.exists?(dst_lib_path)

    if force_reload or lib_missing do
      Logger.debug("transferring cached library from #{module.precompiled} to #{dst_lib_path}")
      # on MacOS, we must delete the old library because otherwise library
      # integrity checker will kill the process
      File.rm(dst_lib_path)
      File.cp!(module.precompiled, dst_lib_path)
    end

    module
  end

  def targets do
    run_zig("targets", [])
  end

  def executable_path do
    # executable_path resolves zig executable in the following fashion:
    #
    # 1. check for zig in `ZIG_ARCHIVE_PATH` env path
    # 2. check for zig cached in `:filename.basedir/3`
    # 3. look for zig using System.find_executable
    #

    cond do
      path = find_from_env() ->
        Logger.info("zig expected (via env variable) in #{path}")
        path

      path = find_in_basedir() ->
        Logger.info("zig expected (via cache) in #{path}")
        path

      path = System.find_executable(zig_cmd_name()) ->
        Logger.info("system zig found in #{path}")
        path

      true ->
        raise CompileError, description: "zig executable not found"
    end
  end

  defp find_from_env do
    cond do
      (path = System.get_env("ZIG_ARCHIVE_PATH", "")) != "" ->
        versioned_path(path)

      (path = System.get_env("ZIG_EXECUTABLE_PATH", "")) != "" ->
        if File.exists?(path), do: path

      :else ->
        nil
    end
  end

  @default_version Zig.version()

  defp find_in_basedir do
    :user_cache
    |> :filename.basedir("zigler")
    |> versioned_path()
  end

  defp versioned_path(path) do
    {os, arch} = Zig.Get.os_info()

    zig_executable = Path.join(path, "zig-#{arch}-#{os}-#{@default_version}/#{zig_cmd_name()}")

    Logger.info("searching for zig in #{zig_executable}")

    if File.exists?(zig_executable), do: zig_executable
  end

  defp precompile_name(prefix) do
    if triple = precompile_meta() do
      triple
      |> Tuple.to_list()
      |> Enum.take(3)
      |> Enum.join("-")
      |> String.replace_prefix("", prefix)
    end
  end

  defp src_lib_name(module) do
    cond do
      windows?() ->
        "#{module}.dll"

      macos?() ->
        "lib#{module}.dylib"

      true ->
        "lib#{module}.so"
    end
  end

  defp dst_lib_name(module) do
    affix = precompile_name(".#{module.version}.")

    if windows?() do
      "#{module.module}#{affix}.dll"
    else
      "#{module.module}#{affix}.so"
    end
  end

  defp zig_cmd_name do
    case :os.type() do
      {_, :nt} -> "zig.exe"
      _ -> "zig"
    end
  end

  # utility function to split on CR(/LF).  This could be platform-dependent.

  def split_on_newline(str) do
    str
    |> String.split("\n")
    |> Enum.map(&String.trim_trailing(&1, "\r"))
  end

  def newline do
    case :os.type() do
      {_, :nt} -> "\r\n"
      _ -> "\n"
    end
  end

  defp macos? do
    case {Target.resolve(), :os.type(), precompile_meta()} do
      {_, _, {_, :macos, _, _}} -> true
      {_, _, {_, _, _, _}} -> false
      {nil, {:unix, :darwin}, nil} -> true
      _ -> false
    end
  end

  defp windows? do
    case {Target.resolve(), :os.type(), precompile_meta()} do
      {_, _, {_, :windows, _, _}} -> true
      {_, _, {_, _, _, _}} -> false
      {nil, {_, :nt}, nil} -> true
      _ -> false
    end
  end

  defp precompile_meta, do: Application.get_env(:zigler, :precompiling)

  defp precompile_callback(filename) do
    case precompile_meta() do
      {_, _, _, callback} ->
        callback.(filename)

      _ ->
        :ok
    end
  end
end
