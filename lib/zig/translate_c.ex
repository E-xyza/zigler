defmodule Zig.TranslateC do
  @moduledoc false

  # Zig 0.17 deprecates the built-in `std.Build.Step.TranslateC` in favour of the
  # ZSF `translate-c` package, which depends in turn on the `aro` C frontend.
  #
  # Both are plain zig source, fetched as source-only git deps (see mix.exs) so that
  # their commits are pinned in mix.lock.  `zig build` cannot consume them straight
  # out of `deps/`, though: translate-c's own build.zig.zon declares aro by git URL,
  # so a build would reach for the network and fail in an airgapped or sandboxed CI.
  #
  # So the two trees are staged once, into a shared directory, with that one
  # declaration rewritten to a relative path dependency.  The staged copy is derived
  # state -- `mix deps.get` rewrites deps/ freely and we re-derive from it.
  #
  # This is deliberately *not* per-module: the two trees are ~23MB across ~1300
  # files, and copying that into every nif's staging directory is slow enough on
  # windows to blow ExUnit's timeout.

  require Logger

  @aro_dep_pattern ~r/\.aro\s*=\s*\.\{\s*\.url\s*=\s*"[^"]+"\s*,\s*\.hash\s*=\s*"[^"]+"\s*,?\s*\},?/s

  @doc """
  Stages translate-c and aro, and returns an absolute path to the translate-c tree
  for use as a zig path dependency.

  The staged trees are shared across every module in a build, and re-derived only
  when the dependency sources are newer.
  """
  def stage! do
    translate_c_src = dep_path!(:translate_c)
    aro_src = dep_path!(:arocc)

    root = cache_directory()
    translate_c_dst = Path.join(root, "translate_c")
    aro_dst = Path.join(root, "aro")

    sync!(translate_c_src, translate_c_dst)
    sync!(aro_src, aro_dst)

    rewrite_aro_dependency!(translate_c_dst)

    translate_c_dst
  end

  @doc """
  Where the staged trees live.  Honours `ZIGLER_STAGING_ROOT` so that a build with a
  relocated staging area keeps everything together.
  """
  def cache_directory do
    # sits directly in the staging root, as a sibling of every module's staging
    # directory, so each generated build.zig.zon can reach it with a relative
    # path -- zig rejects absolute paths in build.zig.zon.
    Path.join(staging_root(), ".translate_c-#{deps_fingerprint()}")
  end

  defp staging_root do
    case System.get_env("ZIGLER_STAGING_ROOT", "") do
      "" -> Zig._tmp_dir()
      path -> path
    end
  end

  # key the staged copy on the dependency revisions, so that a `mix deps.update`
  # lands in a fresh directory instead of being half-overwritten.
  defp deps_fingerprint do
    [:translate_c, :arocc]
    |> Enum.map(fn dep ->
      case Mix.Project.deps_paths()[dep] do
        nil -> "missing"
        path -> path |> File.stat!() |> Map.get(:mtime) |> :erlang.term_to_binary()
      end
    end)
    |> :erlang.term_to_binary()
    |> then(&:crypto.hash(:sha256, &1))
    |> Base.encode16(case: :lower)
    |> binary_part(0, 16)
  end

  defp sync!(src, dst) do
    # the fingerprint already captures dependency changes, so an existing
    # directory is up to date by construction.
    unless File.dir?(dst) do
      tmp = "#{dst}.#{:erlang.unique_integer([:positive])}"
      File.mkdir_p!(Path.dirname(dst))
      File.cp_r!(src, tmp)

      # rename is atomic, so concurrent compilations cannot observe a half-copy
      case File.rename(tmp, dst) do
        :ok -> Logger.debug("staged #{src} to #{dst}")
        {:error, _} -> File.rm_rf!(tmp)
      end
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

      Regex.match?(~r/\.dependencies\s*=\s*\.\{\s*\}/s, zon) ->
        File.write!(
          zon_path,
          String.replace(
            zon,
            ~r/\.dependencies\s*=\s*\.\{\s*\}/s,
            ".dependencies = .{\n    .translate_c = .{.path = \"#{zon_path_escape(translate_c_path)}\"},\n  }"
          )
        )

      Regex.match?(~r/\.dependencies\s*=\s*\.\{/s, zon) ->
        File.write!(
          zon_path,
          String.replace(
            zon,
            ~r/\.dependencies\s*=\s*\.\{/s,
            ".dependencies = .{\n    .translate_c = .{.path = \"#{zon_path_escape(translate_c_path)}\"},",
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

  @doc false
  # zon strings are escaped like zig strings, so windows separators need doubling.
  def zon_path_escape(path), do: String.replace(path, "\\", "\\\\")

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
