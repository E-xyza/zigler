defmodule Zig.C do
  @moduledoc false

  # module creates a struct that defines the options for
  # c-interoperability with zigler

  defstruct include_dirs: [],
            library_dirs: [],
            src: [],
            link_lib: [],
            link_libcpp: false

  alias Zig.Options

  @type t :: %__MODULE__{
          include_dirs: [String.t() | {:system, String.t()}],
          library_dirs: [String.t() | {:system, String.t()}],
          link_lib: [String.t() | {:system, String.t()}],
          link_libcpp: boolean,
          src: [{String.t(), [String.t()]}]
        }

  @type dirpath :: String.t() | {:priv, String.t()} | {:system, String.t()}
  @type dirspec :: dirpath() | [dirpath()]
  @type srcspec :: String.t() | {String.t(), [String.t()]}

  @type opts :: [
          include_dirs: dirspec(),
          library_dirs: dirspec(),
          link_lib: dirspec(),
          link_libcpp: boolean,
          src: srcspec()
        ]

  def new(opts, context) do
    opts
    |> Options.normalize_pathlist(:include_dirs, context)
    |> Options.normalize_pathlist(:library_dirs, context)
    |> Options.normalize_pathlist(:link_lib, context)
    |> Options.validate(:link_libcpp, :boolean, context)
    |> Options.normalize_c_src(:src, context)
    |> then(&struct!(__MODULE__, &1))
  rescue
    e in KeyError ->
      Options.raise_with("was supplied the invalid option `#{e.key}`", context)
  end
end
