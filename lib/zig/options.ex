defmodule Zig.Options do
  @moduledoc """
  parses and normalizes zig options.

  Also sets up

  `options.zig` file which is mapped to `@import("zigler_options")` in
  `beam.zig`.  This is then exposed as `@import("beam").options` in your code.
  """

  alias Zig.EasyC
  alias Zig.Module
  alias Zig.Nif

  @spec normalize!(keyword) :: keyword
  def normalize!(opts) do
    opts
    |> normalize_nifs_option!
    |> normalize_libs
    |> normalize_build_opts
    |> normalize_include_dirs
    |> normalize_c_src
    |> EasyC.normalize_aliasing()
  end

  @common_options ~w[leak_check]a
  @default_options Nif.default_options()

  defp normalize_nifs_option!(opts) do
    easy_c = Keyword.get(opts, :easy_c)

    if easy_c && !Keyword.has_key?(opts, :nifs) do
      raise CompileError, description: "nif specifications are required for easy_c nifs"
    end

    common = Keyword.merge(@default_options, Keyword.take(opts, @common_options))

    opts
    |> Keyword.update(:nifs, {:auto, []}, &Module.normalize_nifs_option!(&1, common, easy_c))
    |> Keyword.put(:default_options, common)
  end

  defp normalize_libs(opts) do
    Keyword.put(opts, :link_lib, List.wrap(opts[:link_lib]))
  end

  @use_gpa {:bool, "use_gpa", true}
  defp normalize_build_opts(opts) do
    # creates build_opts out of a list of build opt shortcuts
    use_gpa = Keyword.get(opts, :use_gpa, false)

    if use_gpa do
      Keyword.update(opts, :build_opts, [@use_gpa], fn list ->
        [@use_gpa | list]
      end)
    else
      opts
    end
  end

  defp normalize_include_dirs(opts) do
    Keyword.update(opts, :include_dir, [], fn
      path_or_paths ->
        path_or_paths
        |> List.wrap()
        |> Enum.map(&absolute_path_for(&1, opts))
    end)
  end

  defp absolute_path_for("/" <> _ = path, _opts), do: path

  defp absolute_path_for(relative_path, opts) do
    opts
    |> Keyword.fetch!(:mod_file)
    |> Path.dirname()
    |> Path.join(relative_path)
  end

  # converts optional c_src option to a list of
  # {path, [<c compiler options>]}
  defp normalize_c_src(opts) do
    Keyword.update(opts, :c_src, [], fn
      path_or_paths ->
        path_or_paths
        |> List.wrap()
        |> Enum.flat_map(&normalize_c_src_paths(&1, opts))
    end)
  end

  defp normalize_c_src_paths({path, c_opts}, opts) do
    unless is_list(c_opts), do: raise("c options for c source files must be a list")

    path
    |> absolute_path_for(opts)
    |> expand_directories
    |> Enum.map(fn file -> {file, c_opts} end)
  end

  defp normalize_c_src_paths(path, opts) when is_binary(path) do
    path
    |> absolute_path_for(opts)
    |> expand_directories
    |> Enum.map(fn file -> {file, []} end)
  end

  defp expand_directories(path) do
    List.wrap(
      if String.ends_with?(path, "/*") do
        path = String.replace_suffix(path, "/*", "")

        path
        |> File.ls!()
        |> Enum.flat_map(fn file ->
          List.wrap(
            if String.ends_with?(file, ".c") or String.ends_with?(file, ".cpp") do
              Path.join(path, file)
            end
          )
        end)
      else
        path
      end
    )
  end
end
