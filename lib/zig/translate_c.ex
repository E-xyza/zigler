defmodule Zig.TranslateC do
  @moduledoc false

  # Zig 0.17 deprecates the built-in `std.Build.Step.TranslateC` in favour of the
  # ZSF `translate-c` package, which depends in turn on the `aro` C frontend.
  #
  # Both are plain zig source, fetched as source-only git deps (see mix.exs) so that
  # their commits are pinned in mix.lock.  `zig build` cannot consume them directly
  # from `deps/`, though: translate-c's own build.zig.zon declares aro by git URL, so
  # a build would reach for the network and fail in an airgapped or sandboxed CI.
  #
  # So the two trees are staged next to the generated build.zig with that one
  # declaration rewritten to a relative path dependency.  The staged copy is derived
  # state -- `mix deps.get` rewrites deps/ freely and we re-derive from it.

  require Logger

  @aro_dep_pattern ~r/\.aro\s*=\s*\.\{\s*\.url\s*=\s*"[^"]+"\s*,\s*\.hash\s*=\s*"[^"]+"\s*,?\s*\},?/s

  @doc """
  Stages translate-c and aro into `staging_directory` and returns the path to the
  translate-c tree, relative to that directory, for use as a zig path dependency.
  """
  def stage!(staging_directory) do
    translate_c_src = dep_path!(:translate_c)
    aro_src = dep_path!(:arocc)

    deps_dir = Path.join(staging_directory, "deps")
    translate_c_dst = Path.join(deps_dir, "translate_c")
    aro_dst = Path.join(deps_dir, "aro")

    sync!(translate_c_src, translate_c_dst)
    sync!(aro_src, aro_dst)

    rewrite_aro_dependency!(translate_c_dst)

    "./deps/translate_c"
  end

  # the vendored trees are large-ish; only re-copy when the source is newer.
  defp sync!(src, dst) do
    if stale?(src, dst) do
      File.rm_rf!(dst)
      File.mkdir_p!(Path.dirname(dst))
      File.cp_r!(src, dst)
      Logger.debug("staged #{src} to #{dst}")
    end
  end

  defp stale?(src, dst) do
    case {File.stat(src), File.stat(dst)} do
      {{:ok, %{mtime: src_mtime}}, {:ok, %{mtime: dst_mtime}}} -> src_mtime > dst_mtime
      {{:ok, _}, _} -> true
      _ -> raise File.Error, reason: :enoent, action: "stage translate-c from", path: src
    end
  end

  # translate-c pins aro by git URL + hash.  Point it at the sibling copy instead, so
  # that `zig build` resolves it locally and never touches the network.
  defp rewrite_aro_dependency!(translate_c_dir) do
    zon_path = Path.join(translate_c_dir, "build.zig.zon")
    zon = File.read!(zon_path)

    cond do
      String.contains?(zon, ~S(.aro = .{ .path = "../aro" })) ->
        :ok

      Regex.match?(@aro_dep_pattern, zon) ->
        File.write!(zon_path, String.replace(zon, @aro_dep_pattern, ~S(.aro = .{ .path = "../aro" },)))

      true ->
        raise CompileError,
          description: """
          could not find the `aro` dependency in translate-c's build.zig.zon.

          Zigler rewrites that declaration to a local path so that nif compilation
          does not require network access.  The upstream package may have changed
          shape; please file an issue at https://github.com/E-xyza/zigler
          """
    end
  end

  @doc """
  Declares the staged translate-c package in a `build.zig.zon` that zigler did not
  generate, so that `build_files_dir` build scripts can use the Translator API.
  """
  def inject_dependency!(zon_path, translate_c_path) do
    zon = File.read!(zon_path)

    cond do
      String.contains?(zon, ".translate_c") ->
        :ok

      # .dependencies = .{} -- empty, possibly with whitespace or a newline
      Regex.match?(~r/\.dependencies\s*=\s*\.\{\s*\}/s, zon) ->
        File.write!(
          zon_path,
          String.replace(
            zon,
            ~r/\.dependencies\s*=\s*\.\{\s*\}/s,
            ".dependencies = .{\n    .translate_c = .{.path = \"#{translate_c_path}\"},\n  }"
          )
        )

      # .dependencies = .{ ...entries... }
      Regex.match?(~r/\.dependencies\s*=\s*\.\{/s, zon) ->
        File.write!(
          zon_path,
          String.replace(
            zon,
            ~r/\.dependencies\s*=\s*\.\{/s,
            ".dependencies = .{\n    .translate_c = .{.path = \"#{translate_c_path}\"},",
            global: false
          )
        )

      true ->
        raise CompileError,
          description: """
          your build.zig.zon (#{zon_path}) has no `.dependencies` block.

          Zigler needs to add the `translate_c` package there so that build.zig can
          translate erl_nif.h.  Add an empty `.dependencies = .{},` and try again.
          """
    end
  end

  defp dep_path!(dep) do
    case Mix.Project.deps_paths()[dep] do
      nil ->
        raise CompileError,
          description: """
          the `#{dep}` dependency is missing.

          Zigler needs it to translate C headers (including erl_nif.h) with Zig 0.17.
          Run `mix deps.get`; if it is still missing, check that your project is not
          overriding zigler's dependencies.
          """

      path ->
        path
    end
  end
end
