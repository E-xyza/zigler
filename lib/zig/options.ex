defmodule Zig.Options do
  @moduledoc """
  parses and normalizes zig options.

  Also sets up

  `options.zig` file which is mapped to `@import("zigler_options")` in
  `beam.zig`.  This is then exposed as `@import("beam").options` in your code.
  """

  alias Zig.EasyC
  alias Zig.Nif

  @spec elixir_normalize!(keyword) :: keyword
  @doc """
  Performs early normalization of options.  For Elixir only, this converts
  all AST representations which must be escaped before moving on to presenting
  options to the Zigler compiler.
  """
  def elixir_normalize!(opts) do
    # if the nifs option exists (explicit specification of nifs), then search
    # through any nifs that define typespecs and macro-escape them.
    opts
    |> replace_lazy(:nifs, &escape_nif_specs/1)
    |> set_auto(opts)
  end

  defp escape_nif_specs(opts) do
    Enum.flat_map(opts, fn
      :auto ->
        []

      {:..., _, nil} ->
        []

      function when is_atom(function) ->
        [{function, []}]

      {nif, nif_opts} ->
        [{nif, escape_spec(nif_opts)}]
    end)
  end

  defp escape_spec(nif_opts) do
    Enum.map(nif_opts, fn
      {:spec, spec} -> {:spec, Macro.escape(spec)}
      other -> other
    end)
  end

  def erlang_normalize!(opts) do
    opts
    |> replace_lazy(:nifs, fn
      :auto -> []
      [:auto | rest] -> rest
      explicit -> explicit
    end)
    |> set_auto(opts)
  end

  def set_auto(new_opts, old_opts) do
    explicit_auto = old_opts
    |> Keyword.get(:nifs)
    |> List.wrap()
    |> Enum.any?(fn
      :auto -> true
      {:..., _, nil} -> true
      _ -> false
    end)

    cond do
      explicit_auto ->
        Keyword.update!(new_opts, :nifs, &{:auto, &1})
      Keyword.get(new_opts, :nifs) ->
        new_opts
      true ->
        # this is the implicit auto case
        Keyword.put(new_opts, :nifs, {:auto, []})
    end
  end

  @spec normalize!(keyword) :: keyword
  def normalize!(opts) do
    opts
    |> normalize_nifs
    |> normalize_libs
    |> normalize_build_opts
    |> normalize_include_dirs
    |> normalize_c_src
    |> EasyC.normalize_aliasing()
  end

  @common_options_keys ~w[leak_check]a
  @default_options Nif.default_options()

  def normalize_nifs(opts) do
    common_options = Keyword.take(opts, @common_options_keys)
    opts = Keyword.merge(opts, default_options: @default_options)

    Keyword.update!(opts, :nifs, fn
      {:auto, opts} ->
        {:auto, Enum.map(opts, &Nif.normalize_options!(&1, common_options))}
      opts ->
        Enum.map(opts, &Nif.normalize_options!(&1, common_options))
    end)
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

  def replace_lazy(opts, key, fun) do
    if Keyword.has_key?(opts, key) do
      Keyword.update!(opts, key, fun)
    else
      opts
    end
  end
end
