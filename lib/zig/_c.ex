defmodule Zig.C do
  @moduledoc false

  # module creates a struct that defines the options for
  # c-interoperability with zigler

  defstruct include_dirs: [],
            library_dirs: [],
            src: [],
            link_lib: [],
            link_libcpp: false

  @type t :: %__MODULE__{
          include_dirs: [Path.t()],
          library_dirs: [Path.t()],
          link_lib: [Path.t()],
          link_libcpp: boolean,
          src: src_opts()
        }

  @type opts :: [
          include_dirs: Path.t() | [Path.t()],
          library_dirs: Path.t() | [Path.t()],
          link_lib: Path.t() | [Path.t()],
          link_libcpp: boolean,
          src: src_opts()
        ]

  @type src_opts :: term

  def new(opts, module_opts) do
    module_dir = cond do
      dir = module_opts[:dir] -> dir
      file = module_opts[:file] -> Path.dirname(file)
    end

    struct!(__MODULE__,
      include_dirs: normalize_filelist(opts, :include_dirs, module_dir),
      library_dirs: normalize_filelist(opts, :library_dirs, module_dir),
      link_lib: normalize_filelist(opts, :link_lib, module_dir),
      link_libcpp: Keyword.get(opts, :link_libcpp, false),
      src: normalized_srclist(opts, module_dir)
    )
  end

  defp normalize_filelist(opts, key, module_dir) do
    opts
    |> Keyword.get(key)
    |> List.wrap()
    |> Enum.map(&solve_relative(&1, module_dir))
  end

  defp normalized_srclist(opts, module_dir) do
    opts
    |> Keyword.get(:src)
    |> List.wrap()
    |> Enum.flat_map(&normalize_src(&1, module_dir))
  end

  defp solve_relative({:system, _} = system, _), do: system

  defp solve_relative(file, module_dir) do
    Path.expand(file, module_dir)
  end

  defp normalize_src(file, module_dir) when is_binary(file) do
    maybe_with_wildcard(file, module_dir, [])
  end

  defp normalize_src({file, opts}, module_dir) do
    maybe_with_wildcard(file, module_dir, opts)
  end

  defp maybe_with_wildcard(file, module_dir, opts) do
    if String.ends_with?(file, "/*") do
      file
      |> Path.dirname()
      |> solve_relative(module_dir)
      |> then(fn wildcard_dir ->
        wildcard_dir
        |> File.ls!()
        |> Enum.map(&{Path.join(wildcard_dir, &1), opts})
      end)
    else
      [{solve_relative(file, module_dir), opts}]
    end
  end
end
