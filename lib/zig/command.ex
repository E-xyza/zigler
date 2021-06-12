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

      zig_download_path = Path.join(@zig_dir_path, archive)
      download_zig_archive(zig_download_path, version, archive)

      # untar the zig directory.
      unarchive_zig(archive)
    end
    :global.del_lock({__MODULE__, self()})
  end

  # https://ziglang.org/download/#release-0.8.0
  @checksums %{
    "zig-freebsd-x86_64-0.8.0.tar.xz" => "0d3ccc436c8c0f50fd55462f72f8492d98723c7218ffc2a8a1831967d81b4bdc",
    "zig-linux-aarch64-0.8.0.tar.xz" => "ee204ca2c2037952cf3f8b10c609373a08a291efa4af7b3c73be0f2b27720470",
    "zig-linux-armv7a-0.8.0.tar.xz" => "d00b8bd97b79f45d6f5da956983bafeaa082e6c2ae8c6e1c6d4faa22fa29b320",
    "zig-linux-i386-0.8.0.tar.xz" => "96e43ee6ed81c3c63401f456bd1c58ee6d42373a43cb324f5cf4974ca0998865",
    "zig-linux-riscv64-0.8.0.tar.xz" => "75997527a78cdab64c40c43d9df39c01c4cdb557bb3992a869838371a204cfea",
    "zig-linux-x86_64-0.8.0.tar.xz" => "502625d3da3ae595c5f44a809a87714320b7a40e6dff4a895b5fa7df3391d01e",
    "zig-macos-x86_64-0.8.0.tar.xz" => "279f9360b5cb23103f0395dc4d3d0d30626e699b1b4be55e98fd985b62bc6fbe",
    "zig-macos-aarch64-0.8.0.tar.xz"	=> "b32d13f66d0e1ff740b3326d66a469ee6baddbd7211fa111c066d3bd57683111",
    "zig-windows-i386-0.8.0.zip" => "a1b9a7421e13153e07fd2e2c93ff29aad64d83105b8fcdafa633dbe689caf1c0",
    "zig-windows-x86_64-0.8.0.zip" => "4818a8a65b4672bc52c0ae7f14d014e0eb8caf10f12c0745176820384cea296a"
  }

  defp download_zig_archive(zig_download_path, version, archive) do
    url = "https://ziglang.org/download/#{version}/#{archive}"
    Logger.info("downloading zig version #{version} (#{url}) and caching in #{@zig_dir_path}.")

    case httpc_get(url) do
      {:ok, %{status: 200, body: body}} ->
        expected_checksum = Map.fetch!(@checksums, archive)
        actual_checksum = :sha256 |> :crypto.hash(body) |> Base.encode16(case: :lower)

        if expected_checksum != actual_checksum do
          raise "checksum mismatch: expected #{expected_checksum}, got #{actual_checksum}"
        end

        File.write!(zig_download_path, body)

      _ ->
        raise "failed to download the appropriate zig archive."
    end
  end

  defp httpc_get(url) do
    {:ok, _} = Application.ensure_all_started(:ssl)
    {:ok, _} = Application.ensure_all_started(:inets)
    headers = []
    request = {String.to_charlist(url), headers}
    http_options = [timeout: 600_000]
    options = [body_format: :binary]

    case :httpc.request(:get, request, http_options, options) do
      {:ok, {{_, status, _}, headers, body}} ->
        {:ok, %{status: status, headers: headers, body: body}}

      other ->
        other
    end
  end

  def unarchive_zig(archive) do
    System.cmd("tar", ["xvf", archive], cd: @zig_dir_path)
  end
end
