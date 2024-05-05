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

  def new(opts) do
    struct!(__MODULE__,
      include_dirs: List.wrap(opts[:include_dirs]),
      link_lib: List.wrap(opts[:link_lib])
    )
  end
end
