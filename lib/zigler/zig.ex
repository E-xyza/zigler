defmodule Zigler.Zig do

  @moduledoc false

  # contains all parts of the Zigler library which is involved in generating zig code.

  alias Zigler.Parser.Nif

  require EEx

  #############################################################################
  ## API

  def compile(compiler, zig_tree) do
    zig_executable = Path.join(zig_tree, "zig")
    zig_rpath = Path.join(zig_tree, "lib/zig")

    include_opts = ["-isystem", Path.join(compiler.staging_dir, "include")]

    [major, minor, patch] = compiler.module_spec.semver
    module = compiler.module_spec.module

    src_file = Path.basename(compiler.code_file)
    cmd_opts = ["build-lib", src_file] ++
      ~w(-dynamic --disable-gen-h --override-lib-dir) ++
      [zig_rpath] ++
      include_opts ++
      ["--ver-major", major, "--ver-minor", minor, "--ver-patch", patch] ++
      #lib_opts ++
      ["--name", "#{module}"] ++
      ["--release-safe"]
      #@release_mode[release_mode]

    opts = [cd: compiler.staging_dir, stderr_to_stdout: true]

    case System.cmd(zig_executable, cmd_opts, opts) do
      {_, 0} -> :ok
      {err, _} ->
        err |> IO.inspect(label: "279")
        raise "error"
        #ErrorParser.parse(err, src_dir, tmp_dir)
    end

    library_filename = Zigler.nif_name(compiler.module_spec)

    # copy the compiled library over to the lib/nif directory.
    File.mkdir_p!(Zigler.nif_dir())
    compiler.staging_dir
    |> Path.join(library_filename)
    |> File.cp!(Path.join(Zigler.nif_dir(), library_filename))

    # link the compiled library to be unversioned.
    symlink_filename = Zigler.nif_dir()
    |> Path.join(Zigler.nif_name(compiler.module_spec, false))
    |> Kernel.<>(".so")

    unless File.exists?(symlink_filename) do
      Zigler.nif_dir()
      |> Path.join(library_filename)
      |> File.ln_s!(symlink_filename)
    end
    :ok
  end

end

