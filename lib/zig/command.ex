defmodule Zig.Command do
  @moduledoc """
  contains all parts of the Zig library involved in calling the
  zig compiler toolchain, especially with regards to the `zig` command, except
  for assembling the build.zig file, which is performed by the
  `Zig.Builder` module.
  """

  alias Zig.Builder

  require Logger

  #############################################################################
  ## API

  def compile(compiler, zig_tree) do
    zig_executable = executable_path(zig_tree)

    opts = [cd: compiler.assembly_dir, stderr_to_stdout: true]

    Logger.debug(
      "compiling nif for module #{inspect(compiler.module_spec.module)} in path #{
        compiler.assembly_dir
      }"
    )

    Builder.build(compiler, zig_tree)

    case System.cmd(zig_executable, ["build"], opts) do
      {_, 0} ->
        :ok

      {err, _} ->
        alias Zig.Parser.Error
        Error.parse(err, compiler)
    end

    lib_dir =
      compiler.module_spec.otp_app
      |> :code.lib_dir()
      |> Path.join("ebin")

    source_library_filename = Zig.nif_name(compiler.module_spec)

    library_filename = maybe_rename_library_filename(source_library_filename)

    # copy the compiled library over to the lib/nif directory.
    File.mkdir_p!(lib_dir)

    compiler.assembly_dir
    |> Path.join("zig-out/lib/#{source_library_filename}")
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

  @local_zig Application.compile_env(:zigler, :local_zig, false)

  defp executable_path(zig_tree), do: executable_path(zig_tree, @local_zig)

  defp executable_path(zig_tree, false), do: Path.join(zig_tree, "zig")
  defp executable_path(_, true), do: System.find_executable("zig")
  defp executable_path(_, path), do: path

  defp maybe_rename_library_filename(fullpath) do
    if Path.extname(fullpath) == ".dylib" do
      fullpath
      |> Path.dirname()
      |> Path.join(Path.basename(fullpath, ".dylib") <> ".so")
    else
      fullpath
    end
  end

  #############################################################################
  ## download zig from online sources.

  @doc false
  def version_name(version) do
    "zig-#{get_os()}-#{get_arch()}-#{version}"
  end

  def get_os do
    case :os.type() do
      {:unix, :linux} ->
        "linux"

      {:unix, :freebsd} ->
        "freebsd"

      {:unix, :darwin} ->
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
    arch =
      :system_architecture
      |> :erlang.system_info()
      |> List.to_string()

    Enum.find_value(@arches, fn
      {prefix, zig_arch} -> if String.starts_with?(arch, prefix), do: zig_arch
    end) || raise arch_warn()
  end

  defp arch_warn,
    do: """
      it seems like you are compiling from an unsupported architecture:
        #{:erlang.system_info(:system_architecture)}
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
      extension =
        if match?({_, :nt}, :os.type()) do
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

  # https://ziglang.org/download/#release-0.9.0
  @checksums %{
    "zig-linux-x86_64-0.9.1.tar.xz" =>
      "be8da632c1d3273f766b69244d80669fe4f5e27798654681d77c992f17c237d7",
    "zig-linux-i386-0.9.1.tar.xz" =>
      "e776844fecd2e62fc40d94718891057a1dbca1816ff6013369e9a38c874374ca",
    "zig-linux-riscv64-0.9.1.tar.xz" =>
      "208dea53662c2c52777bd9e3076115d2126a4f71aed7f2ff3b8fe224dc3881aa",
    "zig-linux-aarch64-0.9.1.tar.xz" =>
      "5d99a39cded1870a3fa95d4de4ce68ac2610cca440336cfd252ffdddc2b90e66",
    "zig-linux-armv7a-0.9.1.tar.xz" =>
      "6de64456cb4757a555816611ea697f86fba7681d8da3e1863fa726a417de49be",
    "zig-macos-x86_64-0.9.1.tar.xz" =>
      "2d94984972d67292b55c1eb1c00de46580e9916575d083003546e9a01166754c",
    "zig-macos-aarch64-0.9.1.tar.xz" =>
      "8c473082b4f0f819f1da05de2dbd0c1e891dff7d85d2c12b6ee876887d438287",
    "zig-windows-x86_64-0.9.1.zip" =>
      "443da53387d6ae8ba6bac4b3b90e9fef4ecbe545e1c5fa3a89485c36f5c0e3a2",
    "zig-windows-i386-0.9.1.zip" =>
      "74a640ed459914b96bcc572183a8db687bed0af08c30d2ea2f8eba03ae930f69",
    "zig-windows-aarch64-0.9.1.zip" =>
      "621bf95f54dc3ff71466c5faae67479419951d7489e40e87fd26d195825fb842",
    "zig-freebsd-x86_64-0.9.1.tar.xz" =>
      "4e06009bd3ede34b72757eec1b5b291b30aa0d5046dadd16ecb6b34a02411254"
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
