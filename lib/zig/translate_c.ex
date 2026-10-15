defmodule Zig.TranslateC do
  @moduledoc false

  # Zig 0.17 deprecates the built-in `std.Build.Step.TranslateC` in favour of the
  # ZSF `translate-c` package, which depends in turn on the `aro` C frontend.
  #
  # Both are vendored under priv/ -- see priv/translate_c/VENDORED.md -- because a
  # hex package may only depend on other hex packages, so neither can be a git
  # dependency of a published zigler, and because compiling a nif should not need
  # network access.
  #
  # `zig build` still cannot read them straight out of priv/: paths in
  # build.zig.zon must be relative to the build root, and priv/ is nowhere near a
  # module staging directory.  So the trees are copied beside the staging
  # directories once per build and referenced from there.
  #
  # That copy is deliberately *not* per-module: the two trees are several thousand
  # files, and copying them into every nif's staging directory is slow enough on
  # windows to blow ExUnit's timeout.

  require Logger

  @doc """
  Stages translate-c and aro, and returns an absolute path to the translate-c tree
  for use as a zig path dependency.

  The staged trees are shared across every module in a build, and re-derived only
  when the dependency sources are newer.
  """
  def stage! do
    root = cache_directory()
    translate_c_dst = Path.join(root, "translate_c")

    sync!(vendored!("translate_c"), translate_c_dst)
    sync!(vendored!("aro"), Path.join(root, "aro"))

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

  # key the staged copy on the vendored sources, so that upgrading zigler lands in
  # a fresh directory instead of half-overwriting the previous one.
  defp deps_fingerprint do
    ["translate_c", "aro"]
    |> Enum.map(fn name ->
      case File.stat(Path.join(:code.priv_dir(:zigler), name)) do
        {:ok, stat} -> stat.mtime
        _ -> :missing
      end
    end)
    |> then(&[Application.spec(:zigler, :vsn) | &1])
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

  defp vendored!(name) do
    path = :zigler |> :code.priv_dir() |> Path.join(name)

    unless File.dir?(path) do
      raise CompileError, description: missing_vendored_message(name, path)
    end

    path
  end

  defp missing_vendored_message(name, path) do
    "zigler's vendored copy of `#{name}` is missing from #{path}.  It ships with " <>
      "the package; if you are working from a git checkout, check that priv/#{name} " <>
      "is present."
  end
end
