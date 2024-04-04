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

    opts =
      Keyword.take(opts, [
        :to,
        :from,
        :link_lib,
        :build_opts,
        :stage1,
        :include_dir,
        :c_src,
        :packages,
        :local_zig
      ])

    opts = Keyword.merge([to: directory], opts)

    opts = if Keyword.has_key?(opts, :link_lib) do
      Keyword.put(opts, :link_lib, adjust_link_lib(opts[:link_lib], opts[:from]))
    end

    #Enum.each(opts[:link_lib], fn 
    #  {:system, _} -> :pass
    #  lib -> 
    #    target = Path.join(opts[:to], lib)
    #    unless File.exists?(target) do
    #      opts[:from]
    #      |> Path.join(lib)
    #      |> File.ln_s!(target)
    #    end
    #end)

    Builder.build(module, opts)
  end

  defp adjust_link_lib(link_lib, from) do
    Enum.map(link_lib, fn
      {:system, _} = system -> system
      lib -> Path.expand(lib, from)
    end)
  end
end
