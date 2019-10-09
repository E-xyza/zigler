defmodule Zigler.Compiler do

  alias Zigler.Zig
  require Logger

  @zig_dir_path Path.expand("../../../zig", __ENV__.file)
  @erl_nif_zig_h Path.join(@zig_dir_path, "include/erl_nif_zig.h")
  @erl_nif_zig_eex File.read!("zig/elixir/erl_nif.zig")

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

  def release_mode_text(:fast), do: ["--release-fast"]
  def release_mode_text(:safe), do: ["--release-safe"]
  def release_mode_text(:small), do: ["--release-small"]
  def release_mode_text(:debug), do: []

  defmacro __before_compile__(context) do
    app = Module.get_attribute(context.module, :zigler_app)
    version = Module.get_attribute(context.module, :zig_version)
    release_mode = Module.get_attribute(context.module, :release_mode)

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
    # now put the elixir.zig file into the temporary directory too.
    elixir_zig_src = Path.join(@zig_dir_path, "elixir/elixir.zig")
    File.cp!(elixir_zig_src, Path.join(tmp_dir, "elixir.zig"))
    # now put the erl_nif.h file into the temporary directory.
    erl_nif_zig_h_path = Path.join(@zig_dir_path, "include/erl_nif_zig.h")
    File.cp!(erl_nif_zig_h_path, Path.join(tmp_dir, "erl_nif_zig.h"))

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
    File.cp(Path.join(tmp_dir, "libzig_nif.so.0.0.0"), Path.join(nif_dir, mod_name <> ".so"))

    quote do
    end
  end
end
