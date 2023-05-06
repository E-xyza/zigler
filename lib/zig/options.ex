defmodule Zig.Options do
  @moduledoc """
  parses and normalizes zig options.
  """

  alias Zig.EasyC
  alias Zig.Module

  @spec normalize(keyword) :: keyword
  def normalize!(opts) do
    opts
    |> normalize_nifs_option!
    |> normalize_libs
    |> normalize_build_opts
    |> EasyC.normalize_aliasing()
  end

  defp normalize_nifs_option!(opts) do
    easy_c = Keyword.get(opts, :easy_c, false)

    if easy_c and !Keyword.has_key?(opts, :nifs) do
      raise CompileError, description: "nif specifications are required for easy_c nifs"
    end

    Keyword.update(:nifs, {:auto, []}, &Module.normalize_nifs_option!(&1, easy_c))
  end

  defp normalize_libs(opts) do
    Keyword.put(opts, :link_lib, List.wrap(opts[:link_lib]))
  end

  @concurrency_modes ~w(dirty_cpu dirty_io threaded yielding)a

  defp normalize_nif_opts(opts) do
    Enum.map(opts, fn
      {:return, return_opts} ->
        normalized =
          return_opts
          |> List.wrap()
          |> normalize_return_opts

        {:return, normalized}

      {:args, args_opts} ->
        {:args, normalize_args_opts(args_opts)}

      concurrency when concurrency in @concurrency_modes ->
        {:concurrency, concurrency}

      other ->
        other
    end)
  end

  @return_types [:charlist, :binary, :default]

  defp normalize_return_opts(opts) do
    Enum.map(opts, fn
      integer when is_integer(integer) -> {:arg, integer}
      type when type in @return_types -> {:type, type}
      :noclean -> {:noclean, true}
      other -> other
    end)
  end

  defp normalize_args_opts(opts) do
    Map.new(
      case opts do
        [{_, _} | _] -> opts
        list -> Enum.with_index(list, fn opt, idx -> {idx, opt} end)
      end
    )
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
