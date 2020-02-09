defmodule Zigler.Compiler do

  @moduledoc false

  require Logger

  alias Zigler.Compiler.ErrorParser
  alias Zigler.Import
  alias Zigler.Zig

  @zig_dir_path Path.expand("../../../zig", __ENV__.file)
  @erl_nif_zig_h Path.join(@zig_dir_path, "include/erl_nif_zig.h")
  @erl_nif_zig Path.join(@zig_dir_path, "beam/erl_nif.zig")

  @doc false
  def basename(version) do
    os = case :os.type do
      {:unix, :linux} ->
        "linux"
      {:unix, :freebsd} ->
        "freebsd"
      {:unix, :darwin} ->
        Logger.warn("macos support is experimental")
        "macos"
      {:win32, _} ->
        Logger.error("windows is definitely not supported.")
        "windows"
    end
    "zig-#{os}-x86_64-#{version}"
  end

  @release_mode %{
    fast:  ["--release-fast"],
    safe:  ["--release-safe"],
    small: ["--release-small"],
    debug: []
  }

  defmacro __before_compile__(context) do
    module = Module.get_attribute(context.module, :zigler)

    zig_tree = Path.join(@zig_dir_path, basename(module.zig_version))

    # check to see if the zig version has been downloaded.
    unless File.dir?(zig_tree) do
      raise CompileError,
        file: context.file,
        line: context.line,
        description: "zig hasn't been downloaded.  Run mix zigler.get_zig #{module.zig_version}"
    end

    #Enum.each(zig_libs, &verify_if_shared/1)
#
    #mod_name = Macro.underscore(context.module)
    #tmp_dir = Path.join("/tmp/.elixir-nifs", mod_name)
    #zig_nif_file = Path.join(tmp_dir, "zig_nif.zig")
#
    #zig_header = Zig.nif_header()
#
    ## count newlines in the header and the code:
    #newlines = count_newlines([zig_header, zig_code]) + 1
#
    #full_code = [zig_header,
    #  zig_code,
    #  "// #{zig_nif_file} line: #{newlines}\n",  #drop in a comment back
    #  Enum.map(zig_specs, &Zig.nif_adapter/1),
    #  zig_longs,
    #  Zig.nif_exports(zig_specs),
    #  Zig.nif_resources(resources),
    #  Zig.nif_footer(context.module, zig_specs, resources)]
#
    #nif_dir = Application.app_dir(app, "priv/nifs")
#
    #File.mkdir_p!(tmp_dir)
#
    #_ = Enum.into(full_code, File.stream!(zig_nif_file))
#
    ## put the erl_nif.zig adapter into the path.
    #File.cp!(@erl_nif_zig, Path.join(tmp_dir, "erl_nif.zig"))
#
    ## now put the beam.zig file into the temporary directory too.
    #beam_zig_src = Path.join(@zig_dir_path, "beam/beam.zig")
    #File.cp!(beam_zig_src, Path.join(tmp_dir, "beam.zig"))
#
    ## included header file operations.
    #src_include_dir = Path.join(src_dir, "include") # NB: this might not exist.
    #dst_include_dir = Path.join(tmp_dir, "include")
    #File.mkdir_p!(dst_include_dir)
#
    ## make sure that erl_nif_zig.h is in the include directory
    #File.cp!(@erl_nif_zig_h, Path.join(dst_include_dir, "erl_nif_zig.h"))
#
    #if File.dir?(src_include_dir) do
    #  src_include_dir
    #  |> File.ls!
    #  |> Enum.each(fn file ->
    #    src_include_dir
    #    |> Path.join(file)
    #    |> File.cp!(Path.join(dst_include_dir, file))
    #  end)
    #end
#
    ## now put the zig import dependencies into the directory, too.
    #zig_code
    #|> IO.iodata_to_binary
    #|> Import.recursive_find(src_dir)
    #|> copy_files(src_dir, tmp_dir, in_test?)
#
    #include_opts = Enum.flat_map([dst_include_dir] ++ c_includes, &["-isystem", &1])
    #lib_opts = Enum.flat_map(zig_libs, &["--library", &1])
#
    ## now use zig to build the library in the temporary directory.
    ## also add in lib search paths that correspond to where we've downloaded
    ## our zig cache
    #zig_cmd = Path.join(zig_tree, "zig")
    #zig_rpath = Path.join(zig_tree, "lib/zig")
    #cmd_opts = ~w(build-lib zig_nif.zig -dynamic --disable-gen-h --override-lib-dir) ++
    #  [zig_rpath] ++
    #  include_opts ++
    #  lib_opts ++
    #  @release_mode[release_mode]
#
    #cmd_opts_text = Enum.join(cmd_opts, " ")
#
    #Logger.info("compiling using command: `#{zig_cmd} #{cmd_opts_text}`")
    #case System.cmd(zig_cmd, cmd_opts, cd: tmp_dir, stderr_to_stdout: true) do
    #  {_, 0} -> :ok
    #  {err, _} ->
    #    raise ErrorParser.parse(err, src_dir, tmp_dir)
    #end
#
    ## move the dynamic library out of the temporary directory and into the priv directory.
#
    ## normally we would make this cp! but having a compiler error here, e.g. stop tests is way
    ## worse than having the module fail to build (which will not stop tests).
    #File.cp(Path.join(tmp_dir, "libzig_nif.so.0.0.0"), Path.join(nif_dir, mod_name <> ".so"))
#
    #mod_path = app
    #|> Application.app_dir("priv/nifs")
    #|> Path.join(Macro.underscore(__CALLER__.module))
#
    #quote do
    #  def __load_nifs__ do
    #    unquote(mod_path)
    #    |> String.to_charlist()
    #    |> :erlang.load_nif(0)
    #  end
    #end

    nif_functions = Enum.map(module.nifs, &function_skeleton/1)

    quote do
      unquote_splicing(nif_functions)

      def __load_nifs__ do
        :ok
      end
    end
  end

  def function_skeleton(%{arity: arity, name: name}) do

    text = "nif for function #{name}/#{arity} not bound"

    params = if arity == 0 do
      []
    else
      for _ <- 1..arity, do: {:_, [], Elixir}
    end

    {:def, [context: Elixir, import: Kernel],
      [
        {name, [context: Elixir], params},
        [do: {:raise, [context: Elixir, import: Kernel], [text]}]
      ]}
  end



  defp copy_files(files, src_dir, dst_dir, in_test?) do

    copy_fn = if in_test?, do: &copy_test/2, else: &File.cp!/2

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

        copy_fn.(src_file_path, dst_file_path)
      end
    end)
  end

  alias Zigler.Unit

  defp copy_test(src_file_path, dst_file_path) do
    modified_content = src_file_path
    |> File.read!
    |> Unit.Parser.modify_file(src_file_path)

    File.write!(dst_file_path, modified_content)
  end

  defp count_newlines([]), do: 0
  defp count_newlines([a | b]) do
    count_newlines(a) + count_newlines(b)
  end
  defp count_newlines(str) when is_binary(str) do
    str |> String.to_charlist |> Enum.count(&(&1 == ?\n))
  end
  defp count_newlines(x) do
    if x == ?\n, do: 1, else: 0
  end

  defp verify_if_shared(lib) do
    if String.ends_with?(lib, ".so") do
      File.exists?(lib) || raise CompileError, description: "shared object library #{lib} not found"
    end
  end

end
