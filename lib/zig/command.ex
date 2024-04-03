defmodule Zig.Command do
  @moduledoc """
  contains all parts of the Zig library involved in calling the
  zig compiler toolchain, especially with regards to the `zig` command, except
  for assembling the build.zig file, which is performed by the
  `Zig.Builder` module.
  """

  alias Zig.Assembler
  alias Zig.Target

  require Logger

  #############################################################################
  ## API

  defp run_zig(command, opts) do
    args = String.split(command)

    base_opts = Keyword.take(opts, [:cd, :stderr_to_stdout])
    zig_cmd = executable_path(opts)
    Logger.debug("running command: #{zig_cmd} #{command}")

    case System.cmd(zig_cmd, args, base_opts) do
      {result, 0} ->
        result

      {error, code} ->
        raise Zig.CompileError, command: command, code: code, error: error
    end
  end

  def run_sema(file, opts) do
    priv_dir = :code.priv_dir(:zigler)
    sema_file = Keyword.get(opts, :sema_context, Path.join(priv_dir, "beam/sema.zig"))
    beam_file = Path.join(priv_dir, "beam/beam.zig")
    erl_nif_file = Path.join(priv_dir, "beam/stub_erl_nif.zig")

    package_opts =
      opts
      |> Keyword.get(:packages)
      |> List.wrap()

    erl_nif_pkg = {:erl_nif, erl_nif_file}

    package_files =
      Enum.map(package_opts, fn {name, {path, _}} -> {name, path} end) ++
        [beam: {beam_file, [erl_nif_pkg]}, erl_nif: erl_nif_file]

    packages =
      Enum.map(package_opts, fn
        {name, path} when is_binary(path) ->
          {name, path}

        {name, {path, []}} ->
          {name, path}

        {name, {path, deps}} ->
          deps_keyword = Enum.map(deps, &{&1, Keyword.fetch!(package_files, &1)})
          {name, {path, deps_keyword}}
      end)

    beam_pkg = {:beam, {beam_file, [erl_nif_pkg]}}

    packages =
      [
        erl_nif_pkg,
        beam_pkg,
        analyte:
          {file,
           [
             beam_pkg,
             erl_nif_pkg
           ] ++ packages}
      ]

    deps =
      packages
      |> package_deps()
      |> String.replace_prefix("", "--deps ")

    mods =
      packages
      |> package_mods()
      |> Enum.join(" ")

    # nerves will put in a `CC` command that we need to bypass because it misidentifies
    # libc locations for statically linking it.
    System.delete_env("CC")

    sema_command = "run #{sema_file} #{deps} #{mods} -lc #{link_opts(opts)}"

    # Need to make this an OK tuple
    {:ok, run_zig(sema_command, Keyword.put(opts, :stderr_to_stdout, true))}
  end

  defp package_deps(packages) do
    packages
    |> Keyword.keys()
    |> Enum.map_join(",", &to_string/1)
  end

  defp package_mods(packages) do
    packages
    |> Enum.flat_map(fn
      {name, {file, deps}} ->
        ["--mod #{name}:#{package_deps(deps)}:#{file}"] ++ package_mods(deps)

      {name, file} ->
        ["--mod #{name}::#{file}"]
    end)
    |> Enum.uniq()
  end

  defp link_opts(opts) do
    opts
    |> Keyword.fetch!(:include_dir)
    |> Enum.map_join(" ", &"-I #{&1}")
  end

  def fmt(file, opts \\ []) do
    run_zig("fmt #{file}", opts)
  end

  def compile(module, opts) do
    assembly_dir = Assembler.directory(module)

    so_dir =
      opts
      |> Keyword.fetch!(:otp_app)
      |> :code.priv_dir()

    lib_dir = Path.join(so_dir, "lib")

    run_zig("build --prefix #{so_dir}", Keyword.put(opts, :cd, assembly_dir))

    src_lib_name = Path.join(lib_dir, src_lib_name(module))
    dst_lib_name = Path.join(lib_dir, dst_lib_name(module))

    # on MacOS, we must delete the old library because otherwise library
    # integrity checker will kill the process
    File.rm(dst_lib_name)
    File.cp!(src_lib_name, dst_lib_name)

    Logger.debug("built library at #{dst_lib_name}")
  end

  def targets do
    run_zig("targets", [])
  end

  defp executable_path(opts) do
    cond do
      opts[:local_zig] -> System.find_executable("zig")
      path = opts[:zig_path] -> path
      true -> Path.join(directory(), "zig")
    end
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

  #############################################################################
  ## download zig from online sources.

  # TODO: move to target using :host as a parameter.

  @doc false
  def os_arch do
    "#{get_os()}-#{get_arch()}"
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
    raise "windows is not supported, and will be supported in zigler 0.11"
  end

  @zig_dir_path Path.expand("../../zig", Path.dirname(__ENV__.file))

  defp directory, do: Path.join(@zig_dir_path, "zig-#{os_arch()}-#{Zig.version()}")

  # TODO: rename this.
  def fetch!(version) do
    zig_dir = directory()
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

      archive = "zig-#{os_arch()}-#{Zig.version()}#{extension}"

      # TODO: clean this up.
      Logger.configure(level: :info)

      zig_download_path = Path.join(@zig_dir_path, archive)
      download_zig_archive(zig_download_path, version, archive)

      # untar the zig directory.
      unarchive_zig(archive)
    end

    :global.del_lock({__MODULE__, self()})
  end

  defp download_zig_archive(zig_download_path, version, archive) do
    url = "https://ziglang.org/download/#{version}/#{archive}"
    Logger.info("downloading zig version #{version} (#{url}) and caching in #{@zig_dir_path}.")

    case httpc_get(url) do
      {:ok, %{status: 200, body: body}} ->
        # expected_checksum = Map.fetch!(@checksums, archive)
        # actual_checksum = :sha256 |> :crypto.hash(body) |> Base.encode16(case: :lower)

        # if expected_checksum != actual_checksum do
        #  raise "checksum mismatch: expected #{expected_checksum}, got #{actual_checksum}"
        # end

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
