defmodule Zig.C do
  @moduledoc false

  # module creates a struct that defines the options for
  # c-interoperability with zigler

  defstruct include_dirs: [],
            library_dirs: [],
            link_lib: [],
            src: [],
            link_libc: true,
            link_libcpp: false

  use Zig.Builder, template: "templates/build_c.zig.eex"

  alias Zig.Options

  @type t :: %__MODULE__{
          include_dirs: [String.t() | {:system, String.t()}],
          library_dirs: [String.t() | {:system, String.t()}],
          link_lib: [String.t() | {:system, String.t()}],
          link_libc: boolean,
          link_libcpp: boolean,
          src: [{String.t(), [String.t()]}]
        }

  @spec new(Zig.c_options(), Options.context()) :: t
  def new(opts, context) do
    opts
    |> Options.scrub_non_keyword(context)
    |> Options.normalize_kw(:include_dirs, [], &normalize_pathlist/2, context)
    |> Options.normalize_kw(:library_dirs, [], &normalize_pathlist/2, context)
    |> Options.normalize_kw(:link_lib, [], &normalize_pathlist/2, context)
    |> Options.normalize_kw(:src, [], &normalize_c_src/2, context)
    |> Options.validate(:link_libcpp, :boolean, context)
    |> then(&struct!(__MODULE__, &1))
  rescue
    e in KeyError ->
      Options.raise_with("was supplied the invalid option `#{e.key}`", context)
  end

  def normalize_pathlist([n | _] = charlist, context) when is_integer(n),
    do: [resolve_path("#{charlist}", context)]

  def normalize_pathlist(path_or_paths, context) do
    path_or_paths
    |> List.wrap()
    |> Enum.map(&resolve_path(&1, context))
  end

  defp resolve_path({:system, path}, context) do
    {:system, IO.iodata_to_binary(path)}
  rescue
    _ in ArgumentError ->
      Options.raise_with("system path must be iodata", path, context)
  end

  defp resolve_path({:priv, path}, context) do
    path
    |> IO.iodata_to_binary()
    |> Path.expand(:code.priv_dir(context.otp_app))
  rescue
    _ in ArgumentError ->
      Options.raise_with("priv path must be iodata", path, context)
  end

  defp resolve_path(path, context) do
    path
    |> IO.iodata_to_binary()
    |> Zig._normalize_path(Path.dirname(context.file))
  rescue
    _ in ArgumentError ->
      Options.raise_with(
        "must be path, `{:priv, path}`, `{:system, path}`, or a list of those",
        path,
        context
      )
  end

  def normalize_c_src([n | _] = charlist, context) when is_integer(n),
    do: [{resolve_path("#{charlist}", context), []}]

  def normalize_c_src(src_or_srcs, context) do
    src_or_srcs
    |> List.wrap()
    |> Enum.flat_map(fn src_def ->
      src_def
      |> resolve_src(context)
      |> expand_wildcards()
    end)
  end

  defp resolve_src({tag, _} = path, context) when tag in ~w[priv system]a do
    path
    |> resolve_path(context)
    |> normalize_src_options([], context)
  end

  defp resolve_src({tag, path, options}, context) when tag in ~w[priv system]a do
    {tag, path}
    |> resolve_path(context)
    |> normalize_src_options(options, context)
  end

  defp resolve_src({path, options}, context) do
    path
    |> IO.iodata_to_binary()
    |> Path.expand(Path.dirname(context.file))
    |> normalize_src_options(options, context)
  rescue
    _ in ArgumentError ->
      Options.raise_with("source files must be iodata", path, context)
  end

  defp resolve_src(path, context) do
    path
    |> IO.iodata_to_binary()
    |> Path.expand(Path.dirname(context.file))
    |> normalize_src_options([], context)
  rescue
    _ in ArgumentError ->
      Options.raise_with(
        "must be path, `{path, [opts]}`, `{:priv, path}`, `{:priv, path, [opts]}`, `{:system, path}`, `{:system, path, [opts]}`, or a list of those",
        path,
        context
      )
  end

  defp normalize_src_options(path, opts, context) when is_list(opts) do
    {path, Enum.map(opts, &IO.iodata_to_binary/1)}
  rescue
    _ in ArgumentError ->
      Options.raise_with(
        "must be a list of iodata",
        opts,
        Options.push_key(context, inspect(path))
      )
  end

  defp expand_wildcards({{:system, path}, opts}) do
    {path, opts}
    |> expand_wildcards()
    |> Enum.map(fn {path, opts} -> {{:system, path}, opts} end)
  end

  defp expand_wildcards({path, opts} = spec) do
    if String.ends_with?(path, "/*") do
      dir = Path.dirname(path)

      dir
      |> File.ls!()
      |> Enum.map(&{Path.join(dir, &1), opts})
    else
      [spec]
    end
  end
end
