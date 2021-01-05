defmodule Zig.Command do

  @moduledoc """
  contains all parts of the Zig library involved in calling the
  zig compiler toolchain, especially with regards to the `zig` command, except
  for assembling the build.zig file, which is performed by the
  `Zig.Builder` module.
  """

  alias Zig.{Builder, Patches}

  require Logger

  #############################################################################
  ## API

  def compile(compiler, zig_tree) do
    zig_executable = if compiler.module_spec.local_zig do
      System.find_executable("zig")
    else
      # apply patches, if applicable
      Patches.sync(zig_tree)
      Path.join(zig_tree, "zig")
    end

    opts = [cd: compiler.assembly_dir, stderr_to_stdout: true]

    Logger.debug("compiling nif for module #{inspect compiler.module_spec.module} in path #{compiler.assembly_dir}")

    Builder.build(compiler, zig_tree)

    case System.cmd(zig_executable, ["build"], opts) do
      {_, 0} -> :ok
      {err, _} ->
        alias Zig.Parser.Error
        Error.parse(err, compiler)
    end

    lib_dir = compiler.module_spec.otp_app
    |> :code.lib_dir()
    |> Path.join("ebin")

    library_filename = Zig.nif_name(compiler.module_spec)

    # copy the compiled library over to the lib/nif directory.
    File.mkdir_p!(lib_dir)

    compiler.assembly_dir
    |> Path.join("zig-cache/lib/#{library_filename}")
    |> File.cp!(Path.join(lib_dir, library_filename))

    # link the compiled library to be unversioned.
    symlink_filename = Path.join(lib_dir, "#{library_filename}")

    unless File.exists?(symlink_filename) do
      lib_dir
      |> Path.join(library_filename)
      |> File.ln_s!(symlink_filename)
    end
    :ok
  end

  #############################################################################
  ## download zig from online sources.

  @doc false
  def version_name(version) do
    "zig-#{get_os()}-#{get_arch()}-#{version}"
  end

  def get_os do
    case :os.type do
      {:unix, :linux} ->
        "linux"
      {:unix, :freebsd} ->
        "freebsd"
      {:unix, :darwin} ->
        Logger.warn("macos support is experimental")
        "macos"
      {_, :nt} ->
        windows_warn()
        "windows"
    end
  end

  @arches %{
    "i386" => "i386",
    "i486" => "i386",
    "i586" => "i386",
    "x86_64" => "x86_64",
    "armv6" => "armv6kz",
    "armv7" => "armv7a",
    "aarch64" => "aarch64",
    "amd64" => "x86_64",
    "win32" => "i386",
    "win64" => "x86_64"
  }

  # note this is the architecture of the machine where compilation is
  # being done, not the target architecture of cross-compiled
  def get_arch do
    arch = :system_architecture
    |> :erlang.system_info()
    |> List.to_string()

    Enum.find_value(@arches, fn
      {prefix, zig_arch} -> if String.starts_with?(arch, prefix), do: zig_arch
    end) || raise arch_warn()
  end

  defp arch_warn, do: """
    it seems like you are compiling from an unsupported architecture:
      #{ :erlang.system_info(:system_architecture) }
    Please leave an issue at https://github.com/ityonemo/zigler/issues
  """

  defp windows_warn do
    Logger.warn("""
        windows is not supported, but may work.

        If you find an error in the process, please leave an issue at:
        https://github.com/ityonemo/zigler/issues
        """)
  end

  @zig_dir_path Path.expand("../../zig", Path.dirname(__ENV__.file))

  def fetch(version) do
    zig_dir = Path.join(@zig_dir_path, version_name(version))
    zig_executable = Path.join(zig_dir, "zig")
    :global.set_lock({__MODULE__, self()})
    unless File.exists?(zig_executable) do
      # make sure the zig directory path exists and is ready.
      File.mkdir_p!(@zig_dir_path)

      # make sure that we're in the correct operating system.
      extension = if match?({_, :nt}, :os.type()) do
        ".zip"
      else
        ".tar.xz"
      end

      archive = version_name(version) <> extension

      Logger.configure(level: :info)
      Application.ensure_all_started(:mojito)

      zig_download_path = Path.join(@zig_dir_path, archive)

      Logger.info("downloading zig version #{version} (#{archive}) and caching in #{@zig_dir_path}.")
      download_location = "https://ziglang.org/download/#{version}/#{archive}"
      download_zig_archive(zig_download_path, download_location)

      # untar the zig directory.
      unarchive_zig(archive)
    end
    :global.del_lock({__MODULE__, self()})
  end

  def download_zig_archive(zig_download_path, download_location) do
    Application.ensure_all_started(:ssl)

    case Mojito.get(download_location, [], pool: false, timeout: 600_000) do
      {:ok, download = %{status_code: 200}} ->
        File.write!(zig_download_path, download.body)
      _ -> raise "failed to download the appropriate zig archive."
    end
  end

  def unarchive_zig(archive) do
    System.cmd("tar", ["xvf", archive], cd: @zig_dir_path)
  end
end
