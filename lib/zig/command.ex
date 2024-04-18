defmodule Zig.Command do
  @moduledoc """
  contains all parts of the Zig library involved in calling the
  zig compiler toolchain, especially with regards to the `zig` command, except
  for assembling the build.zig file, which is performed by the
  `Zig.Builder` module.
  """

  alias Zig.Builder
  alias Zig.Target

  require Logger

  #############################################################################
  ## API

  defp run_zig(command, module) do
    args = String.split(command)

    base_opts = Keyword.take(module, [:cd, :stderr_to_stdout])
    zig_cmd = executable_path(module)
    Logger.debug("running command: #{zig_cmd} #{command}")

    case System.cmd(zig_cmd, args, base_opts) do
      {result, 0} ->
        result

      {error, code} ->
        raise Zig.CompileError, command: command, code: code, error: error
    end
  end

  def run_sema!(file, module) do
    # TODO: add availability of further options here.
    # TODO: make this an eex file.
    
    dbg()
    
    priv_dir = :code.priv_dir(:zigler)
    sema_file = Path.join(priv_dir, "beam/sema.zig")
    beam_file = Path.join(priv_dir, "beam/beam.zig")
    erl_nif_file = Path.join(priv_dir, "beam/stub_erl_nif.zig")

    package_opts = module.packages

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

    sema_command = "run #{sema_file} #{deps} #{mods} -lc #{link_opts(module)}"

    run_zig(sema_command, stderr_to_stdout: true)
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

  defp link_opts(module) do
    Enum.map_join(module.include_dir, " ", &"-I #{&1}")
  end

  def fmt(file) do
    run_zig("fmt #{file}", [])
  end

  def compile!(module) do
    staging_directory = Builder.staging_directory(module.module)

    so_dir = :code.priv_dir(module.otp_app)

    lib_dir = Path.join(so_dir, "lib")

    run_zig("build --prefix #{so_dir}", cd: staging_directory)

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

  defp executable_path(module) do
    # executable_path resolves zig executable in the following fashion:
    #
    # 1. check for zig in `ZIG_ARCHIVE_PATH` env path
    # 2. check for zig cached in `:filename.basedir/3`
    # 3. look for zig using System.find_executable
    #

    cond do
      path = find_from_env() ->
        path

      path = find_in_basedir() ->
        path

      path = System.find_executable("zig") ->
        path

      true ->
        raise CompileError, description: "zig executable not found"
    end
  end

  defp find_from_env do
    if (path = System.get_env("ZIG_ARCHIVE_PATH", "")) != "" do
      versioned_path(path)
    end
  end

  defp find_in_basedir do
    :user_cache
    |> :filename.basedir("zigler")
    |> versioned_path()
  end

  defp versioned_path(path) do
    {os, arch} = os_info()

    zig_executable = Path.join(path, "zig-#{os}-#{arch}-#{@default_version}/zig")
    if File.exists?(zig_executable), do: zig_executable
  end

  defp os_info do
    :system_architecture
    |> :erlang.system_info()
    |> to_string
    |> String.split("-")
    |> decode_os_info()
  end

  defp decode_os_info([arch, _vendor, os | _]), do: {os, arch}

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
end
