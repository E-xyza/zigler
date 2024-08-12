defmodule Zig.C do
  @moduledoc false

  # module creates a struct that defines the options for
  # c-interoperability with zigler

  defstruct include_dirs: [],
            src: [],
            link_lib: [],
            link_libcpp: false

  @type t :: %__MODULE__{
          include_dirs: [Path.t()],
          link_lib: [Path.t()],
          link_libcpp: boolean,
          src: src_opts()
        }

  @type opts :: [
          include_dirs: Path.t() | [Path.t()],
          link_lib: Path.t() | [Path.t()],
          link_libcpp: boolean,
          src: src_opts()
        ]

  @type src_opts :: term

  def new(opts, module_file) do
    struct!(__MODULE__,
      include_dirs: normalize_filelist(opts, :include_dirs, module_file),
      link_lib: normalize_filelist(opts, :link_lib, module_file),
      link_libcpp: Keyword.get(opts, :link_libcpp, false),
      src: normalized_srclist(opts, module_file)
    )
  end

  defp normalize_filelist(opts, key, module_file) do
    opts
    |> Keyword.get(key)
    |> List.wrap()
    |> Enum.map(&solve_relative(&1, module_file))
  end

  defp normalized_srclist(opts, module_file) do
    opts
    |> Keyword.get(:src)
    |> List.wrap()
    |> Enum.flat_map(&normalize_src(&1, module_file))
  end

  defp solve_relative({:system, _} = system, _), do: system

  defp solve_relative(file, module_file) do
    Path.expand(file, module_file)
  end

  defp normalize_src(file, module_file) when is_binary(file) do
    maybe_with_wildcard(file, module_file, [])
  end

  defp normalize_src({file, opts}, module_file) do
    maybe_with_wildcard(file, module_file, opts)
  end

  defp maybe_with_wildcard(file, module_file, opts) do
    if String.ends_with?(file, "/*") do
      file
      |> solve_relative(module_file)
      |> Path.dirname()
      |> then(fn wildcard_dir ->
        wildcard_dir
        |> File.ls!()
        |> Enum.map(&{Path.join(wildcard_dir, &1), opts})
      end)
    else
      [{solve_relative(file, module_file), opts}]
    end
  end
end
