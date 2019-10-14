defmodule Zigler.Compiler do

  alias Zigler.Zig
  alias Zigler.Parser
  require Logger

  @zig_dir_path Path.expand("../../../zig", __ENV__.file)
  @erl_nif_zig_h Path.join(@zig_dir_path, "include/erl_nif_zig.h")
  @erl_nif_zig_eex File.read!("zig/beam/erl_nif.zig")

  def basename(version) do
    os = case :os.type do
      {:unix, :linux} ->
        "linux"
      {:unix, :darwin} ->
        Logger.warn("macos support is experimental")
        "macos"
      {:unix, :freebsd} ->
        Logger.error("freebsd is not supported.")
        "freebsd"
      {:win32, _} ->
        Logger.error("windows is definitely not supported.")
        "windows"
    end
    "zig-#{os}-x86_64-#{version}"
  end

  # TODO: make this a map!
  def release_mode_text(:fast), do: ["--release-fast"]
  def release_mode_text(:safe), do: ["--release-safe"]
  def release_mode_text(:small), do: ["--release-small"]
  def release_mode_text(:debug), do: []

  defmacro __before_compile__(context) do
    app = Module.get_attribute(context.module, :zigler_app)
    version = Module.get_attribute(context.module, :zig_version)
    release_mode = Module.get_attribute(context.module, :release_mode)

    code_dir = Path.dirname(__CALLER__.file)

    zig_tree = Path.join(@zig_dir_path, basename(version))
    # check to see if the zig version has been downloaded.
    unless File.dir?(zig_tree), do: raise "zig hasn't been downloaded.  Run mix zigler.get_zig #{version}"

    zig_code = Module.get_attribute(context.module, :zig_code)
    zig_specs = Module.get_attribute(context.module, :zig_specs)
    |> Enum.flat_map(&(&1))

    full_code = [Zig.nif_header(), zig_code,
      Enum.map(zig_specs, &Zig.nif_adapter/1),
      Zig.nif_exports(zig_specs),
      Zig.nif_footer(context.module, zig_specs)]

    mod_name = context.module |> Atom.to_string |> String.downcase
    tmp_dir = Path.join("/tmp/.elixir-nifs", mod_name)
    nif_dir = Application.app_dir(app, "priv/nifs")

    File.mkdir_p!(tmp_dir)

    Enum.into(full_code, File.stream!(Path.join(tmp_dir, "zig_nif.zig")))

    # put the erl_nif.zig adapter into the path.
    erl_nif_zig_path = Path.join(tmp_dir, "erl_nif.zig")
    File.write!(erl_nif_zig_path, EEx.eval_string(@erl_nif_zig_eex, erl_nif_zig_h: @erl_nif_zig_h))
    # now put the beam.zig file into the temporary directory too.
    beam_zig_src = Path.join(@zig_dir_path, "beam/beam.zig")
    File.cp!(beam_zig_src, Path.join(tmp_dir, "beam.zig"))
    # now put the erl_nif.h file into the temporary directory.
    erl_nif_zig_h_path = Path.join(@zig_dir_path, "include/erl_nif_zig.h")
    File.cp!(erl_nif_zig_h_path, Path.join(tmp_dir, "erl_nif_zig.h"))
    # now put the zig import dependencies into the file.
    zig_code
    |> Parser.imports
    |> copy_files(code_dir, tmp_dir)

    # now use zig to build the library in the temporary directory.
    # also add in lib search paths that correspond to where we've downloaded
    # our zig cache
    zig_cmd = Path.join(zig_tree, "zig")
    zig_rpath = Path.join(zig_tree, "lib/zig")
    cmd_opts = ~w(build-lib zig_nif.zig -dynamic --disable-gen-h --override-lib-dir) ++ [
      zig_rpath | release_mode_text(release_mode)]

    cmd_opts_text = Enum.join(cmd_opts, " ")

    Logger.info("compiling using command: `#{zig_cmd} #{cmd_opts_text}`")
    System.cmd(zig_cmd, cmd_opts, cd: tmp_dir)

    # move the dynamic library out of the temporary directory and into the priv directory.

    # normally we would make this cp! but having a compiler error here, e.g. stop tests is way
    # worse than having the module fail to build (which will not stop tests).
    File.cp(Path.join(tmp_dir, "libzig_nif.so.0.0.0"), Path.join(nif_dir, mod_name <> ".so"))

    quote do
    end
  end

  defp copy_files(files, src_dir, dst_dir) do
    Enum.each(files, fn file_path ->
      src_file_path = Path.join(src_dir, file_path) |> Path.expand()
      dst_file_path = Path.join(dst_dir, file_path)
      # we might be trying to import something which doesn't exist as
      # a relative file (e.g. `std`); in that case just don't complain,
      # but also don't copy.
      if File.exists?(src_file_path) do
        # make sure our target directory exists.
        dst_file_path
        |> Path.dirname
        |> File.mkdir_p!

        # copy the file.
        File.cp!(src_file_path, dst_file_path)
        # now that we've copied this file, we need to recursively enter it
        # and make sure that its dependencies are OK.
        new_src_path = Path.dirname(src_file_path)
        new_dst_path = Path.dirname(dst_file_path)

        src_file_path
        |> File.read!
        |> Parser.imports
        |> copy_files(new_src_path, new_dst_path)
      end
    end)
  end
end
