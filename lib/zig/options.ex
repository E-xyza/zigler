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
    |> EasyC.normalize_aliasing()
  end

  @common_options ~w(leak_check)a
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
end
