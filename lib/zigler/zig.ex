defmodule Zigler.Zig do

  @moduledoc false

  # contains all parts of the Zigler library which is involved in calling the
  # zig compiler toolchain.

  alias Zigler.Patches

  require Logger

  #############################################################################
  ## API

  def compile(compiler, zig_tree) do
    # apply patches, if applicable
    Patches.sync(zig_tree)

    zig_executable = Path.join(zig_tree, "zig")
    zig_rpath = Path.join(zig_tree, "lib/zig")

    include_opts = ["-isystem", Path.join(compiler.assembly_dir, "include")] ++
      includes_from_module(compiler.module_spec)

    lib_opts = libraries_from_module(compiler.module_spec)

    version = compiler.module_spec.version
    module = compiler.module_spec.module

    src_file = Path.basename(compiler.code_file)
    cmd_opts = ["build-lib", src_file] ++
      ~w(-dynamic -lc) ++ cross_compile(compiler.module_spec) ++
      ~w(--disable-gen-h --override-lib-dir) ++
      [zig_rpath] ++
      include_opts ++
      ["--ver-major", "#{version.major}",
       "--ver-minor", "#{version.minor}",
       "--ver-patch", "#{version.patch}"] ++
      lib_opts ++
      ["--name", "#{module}"] ++
      ["--release-safe"]
      #@release_mode[release_mode]

    opts = [cd: compiler.assembly_dir, stderr_to_stdout: true]

    case System.cmd(zig_executable, cmd_opts, opts) do
      {_, 0} -> :ok
      {err, _} ->
        alias Zigler.Parser.Error
        Error.parse(err, compiler)
    end

    lib_dir = compiler.module_spec.otp_app
    |> :code.lib_dir()
    |> Path.join("ebin")

    library_filename = Zigler.nif_name(compiler.module_spec)

    # copy the compiled library over to the lib/nif directory.
    File.mkdir_p!(lib_dir)

    compiler.assembly_dir
    |> Path.join(library_filename)
    |> File.cp!(Path.join(lib_dir, library_filename))

    # link the compiled library to be unversioned.
    symlink_filename = lib_dir
    |> Path.join(Zigler.nif_name(compiler.module_spec, false))
    |> Kernel.<>(".so")

    unless File.exists?(symlink_filename) do
      lib_dir
      |> Path.join(library_filename)
      |> File.ln_s!(symlink_filename)
    end
    :ok
  end

  # currently all targets are arm and use linux 4.19
  @arm419 ~w(-target arm-linux.4.19-gnueabihf)
  @cross_settings %{
    host: [], rpi: @arm419, rpi0: @arm419, rpi2: @arm419,
    rpi3: @arm419, rpi3a: @arm419, rpi4: @arm419, bbb: @arm419
  }

  defp cross_compile(%{target: target}) when is_binary(target) do
    ["-target", target]
  end
  defp cross_compile(_) do
    alias Mix.Nerves.Utils
    utils = function_exported?(Utils, :mix_target, 0) and Utils
    if utils, do: @cross_settings[utils.mix_target()], else: []
  end

  #############################################################################
  ## INCLUDES

  @spec includes_from_module(Module.t) :: [Path.t]
  defp includes_from_module(module) do
    (module.c_includes
    |> Keyword.values
    |> Enum.flat_map(&include_directories/1))
    ++
    Enum.flat_map(module.include_dirs, &["-isystem", &1])
  end

  @spec include_directories([Path.t] | Path.t) :: [Path.t]
  def include_directories(path) when is_binary(path) do
    case Path.dirname(path) do
      "." -> []
      path -> ["-isystem", path]
    end
  end
  def include_directories(paths) when is_list(paths) do
    Enum.flat_map(paths, &include_directories/1)
  end

  #############################################################################
  ## LIBRARIES

  defp libraries_from_module(module) do
    Enum.flat_map(module.libs, &["--library", &1])
  end

  #############################################################################
  ## download zig from online sources.

  @doc false
  def version_name(version) do
    os = case :os.type do
      {:unix, :linux} ->
        "linux"
      {:unix, :freebsd} ->
        "freebsd"
      {:unix, :darwin} ->
        Logger.warn("macos support is experimental")
        "macos"
      {:win32, _} ->
        Logger.warn("windows is not supported, but may work.")
        "windows"
    end

    arch = case :erlang.system_info(:system_architecture) do
      [?i, _] ++ '86' ++ _ -> "i386"
      'x86_64' ++ _ -> "x86_64"
      'armv6' ++ _ -> "armv6kz"
      'armv7' ++ _ -> "armv7a"
      'aarch64' ++ _ -> "aarch64"
      _ -> raise """
      it seems like you are compiling from an unsupported architecture.
      Please leave an issue at https://github.com/ityonemo/zigler/issues
      """
    end

    "zig-#{os}-#{arch}-#{version}"
  end

  @zig_dir_path Path.expand("../zig", Path.dirname(__ENV__.file))

  def fetch(version) do
    # make sure that we're in the correct operating system.
    extension = if match?({:win32, _}, :os.type()) do
      Logger.warn("""
      windows is not currently supported.  If you find an error
      in the process, please leave an issue at https://github.com/ityonemo/zigler/issues
      """)
      "zip"
    else
      "tar.xz"
    end

    Logger.configure(level: :info)
    Application.ensure_all_started(:mojito)

    tarfile = version_name(version) <> extension
    # make sure the zig directory path exists and is ready.
    File.mkdir_p!(@zig_dir_path)
    zig_download_path = Path.join(@zig_dir_path, tarfile)

    unless File.exists?(zig_download_path) do
      Logger.info("downloading zig version #{version} and caching in #{@zig_dir_path}.")
      download_location = "https://ziglang.org/download/#{version}/#{tarfile}"

      download_zig_tarball(zig_download_path, download_location)
    end

    # untar the zig directory.
    zig_version_cache = Path.join(@zig_dir_path, version_name(version))

    unless File.dir?(zig_version_cache) do
      System.cmd("tar", ["xvf", tarfile], cd: @zig_dir_path)
    end
  end

  def download_zig_tarball(zig_download_path, download_location) do
    Application.ensure_all_started(:ssl)
    case Mojito.get(download_location, [], pool: false, timeout: 100_000) do
      {:ok, download = %{status_code: 200}} ->
        File.write!(zig_download_path, download.body)
      _ -> raise "failed to download the appropriate zig binary."
    end
  end
end
