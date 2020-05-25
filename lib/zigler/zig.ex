defmodule Zigler.Zig do

  @moduledoc false

  # contains all parts of the Zigler library which is involved in calling the
  # zig compiler toolchain.

  #############################################################################
  ## API

  def compile(compiler, zig_tree) do
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

end
