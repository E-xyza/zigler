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

  defp run_zig(command, opts) do
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
    # c = maybe_add_windows_shim(opts[:c])

    # nerves will put in a `CC` command that we need to bypass because it misidentifies
    # libc locations for statically linking it.
    System.delete_env("CC")

    run_zig("build -Dzigler-mode=sema sema --summary none",
      cd: Path.dirname(module.module_code_path),
      stderr_to_stdout: true
    )
  end

  def fmt(file) do
    if System.get_env("ZIG_FMT", "true") != "false" do
      run_zig("fmt #{file}", [])
    end
  end

  def compile!(module) do
    staging_directory = Builder.staging_directory(module.module)

    so_dir = :code.priv_dir(module.otp_app)

    lib_dir = Path.join(so_dir, "lib")

    run_zig("build -Doptimize=#{release_mode(module)} --prefix #{so_dir}",
      cd: staging_directory,
      stderr_to_stdout: true
    )

    case :os.type() do
      {_, :nt} ->
        # windows dlls wind up in the bin directory instead of the lib directory.
        bin_dir = Path.join(so_dir, "bin")
        src_lib_path = Path.join(bin_dir, src_lib_name(module.module))
        dst_lib_path = Path.join(lib_dir, dst_lib_name(module.module))

        # on Windows, not deleting this causes a file permissions error.
        File.rm(dst_lib_path)
        File.cp!(src_lib_path, dst_lib_path)

        Logger.debug("built library at #{dst_lib_path}")

      _ ->
        src_lib_path = Path.join(lib_dir, src_lib_name(module.module))
        dst_lib_path = Path.join(lib_dir, dst_lib_name(module.module))

        # on MacOS, we must delete the old library because otherwise library
        # integrity checker will kill the process
        File.rm(dst_lib_path)
        File.cp!(src_lib_path, dst_lib_path)

        Logger.debug("built library at #{dst_lib_path}")
    end

    module
  rescue
    e in Zig.CompileError ->
      reraise Zig.CompileError.resolve(e, module), __STACKTRACE__
  end

  def targets do
    run_zig("targets", [])
  end

  @release_modes %{
    debug: "Debug",
    fast: "ReleaseFast",
    small: "ReleaseSmall",
    safe: "ReleaseSafe"
  }

  def release_mode(%{release_mode: :env}) do
    System.fetch_env!("ZIGLER_RELEASE_MODE")
  end

  def release_mode(%{release_mode: {:env, mode}}) do
    case System.get_env("ZIGLER_RELEASE_MODE") do
      env when env in ["", nil] ->
        Map.fetch!(@release_modes, mode)

      supplied ->
        supplied
    end
  end

  def release_mode(module) do
    Map.fetch!(@release_modes, module.release_mode)
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

  defp src_lib_name(module) do
    case {Target.resolve(), :os.type()} do
      {nil, {:unix, :darwin}} ->
        "lib#{module}.dylib"

      {nil, {_, :nt}} ->
        "#{module}.dll"

      _ ->
        "lib#{module}.so"
    end
  end

  defp dst_lib_name(module) do
    case {Target.resolve(), :os.type()} do
      {nil, {:unix, :darwin}} ->
        "#{module}.so"

      {nil, {_, :nt}} ->
        "#{module}.dll"

      _ ->
        "#{module}.so"
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
end
