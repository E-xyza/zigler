defmodule Zig.Patches do
  @moduledoc """
  occasionally zig and/or the llvm bits that get
  pulled down have errors.  This module actively patches
  code when this situation occurs.
  """

  require Logger

  # currently no patches identified for zig 0.7.1
  @patches %{}

  def sync(zig_path) do
    @patches
    |> Enum.filter(&patch_needed?(&1, zig_path))
    |> Enum.each(&patch(&1, zig_path))
  end

  def patch_needed?({src_rel_path, _}, zig_path) do
    zig_path
    |> Path.join(src_rel_path)
    |> File.exists?()
  end

  defp patch({src_rel_path, patch_filename}, zig_path) do
    src_path = Path.join(zig_path, src_rel_path)

    patch_path = :zigler
    |> :code.priv_dir
    |> Path.join("patches/#{patch_filename}")

    # check for equality.  Might want to do something
    # more effective than this.
    src_sha256 = :crypto.hash(:sha256, File.read!(src_path))
    patch_sha256 = :crypto.hash(:sha256, File.read!(patch_path))

    unless src_sha256 == patch_sha256 do
      Logger.warn("#{src_path} needed to be patched.")
      File.cp!(patch_path, src_path)
    end
  end

end
