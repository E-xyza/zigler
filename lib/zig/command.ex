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

    opts = Keyword.merge(hacky_envs(), cd: compiler.assembly_dir, stderr_to_stdout: true)

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

  @local_zig Application.get_env(:zigler, :local_zig, false)

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

  # REVIEW THIS ON ZIG 1.0.0
  defp hacky_envs do
    List.wrap(
      if :os.type() == {:unix, :darwin} do
        [env: [{"ZIG_SYSTEM_LINKER_HACK", "true"}]]
      end
    )
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
    "zig-freebsd-x86_64-0.8.1.tar.xz" =>
      "fc4f6478bcf3a9fce1b8ef677a91694f476dd35be6d6c9c4f44a8b76eedbe176",
    "zig-linux-aarch64-0.8.1.tar.xz" =>
      "2166dc9f2d8df387e8b4122883bb979d739281e1ff3f3d5483fec3a23b957510",
    "zig-linux-armv7a-0.8.1.tar.xz" =>
      "5ba58141805e2519f38cf8e715933cbf059f4f3dade92c71838cce341045de05",
    "zig-linux-i386-0.8.1.tar.xz" =>
      "2f3e84f30492b5f1c5f97cecc0166f07a8a8d50c5f85dbb3a6ef2a4ee6f915e6",
    "zig-linux-riscv64-0.8.1.tar.xz" =>
      "4adfaf147b025917c03367462fe5018aaa9edbc6439ef9cd0da2b074ae960554",
    "zig-linux-x86_64-0.8.1.tar.xz" =>
      "6c032fc61b5d77a3f3cf781730fa549f8f059ffdb3b3f6ad1c2994d2b2d87983",
    "zig-macos-x86_64-0.8.1.tar.xz" =>
      "16b0e1defe4c1807f2e128f72863124bffdd906cefb21043c34b673bf85cd57f",
    "zig-macos-aarch64-0.8.1.tar.xz" =>
      "5351297e3b8408213514b29c0a938002c5cf9f97eee28c2f32920e1227fd8423",
    "zig-windows-i386-0.8.1.zip" =>
      "099605051eb0452a947c8eab8fbbc7e43833c8376d267e94e41131c289a1c535",
    "zig-windows-x86_64-0.8.1.zip" =>
      "43573db14cd238f7111d6bdf37492d363f11ecd1eba802567a172f277d003926"
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
