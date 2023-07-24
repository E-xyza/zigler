defmodule Zig.Assembler do
  @moduledoc """
  Assembles the staging directory for zig content.

  The staging directory contains:

  - `build.zig` for the library file.
  """

  alias Zig.Builder

  @doc "staging directory for the zigler assembly"
  def directory(module) do
    Path.join(System.tmp_dir(), to_string(module))
  end

  def assemble(module, opts) do
    directory = directory(module)
    File.mkdir_p!(directory)

    opts = Keyword.take(opts, [:to, :from, :link_lib, :build_opts, :stage1, :include_dir, :c_src, :packages])
    opts = Keyword.merge([to: directory], opts)

    Builder.build(module, opts)
  end
end
