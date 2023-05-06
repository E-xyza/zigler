defmodule Zig.Module do
  @moduledoc """
  abstraction representing multiple zig nif functions bound into a single
  module
  """

  alias Zig.Nif

  @doc """
  obtains a list of Nif structs from the semantically analyzed content and
  the nif options that are a part of
  """
  # TODO: unit test this function directly.
  def nifs_from_sema(sema_list, nif_opts) do
    Enum.map(nif_opts, fn
      {name, opts} ->
        # "calculated" details.
        function =
          sema_list
          |> find_function(name)
          |> adopt_options(opts)

        concurrency = Keyword.get(opts, :concurrency, :synchronous)
        concurrency_module = Map.get(@concurrency_modules, concurrency)

        Nif.new(opts, function)
    end)
  end

  #############################################################################
  ## OPTIONS NORMALIZATION

  @doc false
  def normalize_nifs_option!({:auto, nifs}, false) do
    # for erlang: we can specify that we want to automatically find functions,
    # but override the list of functions provided using the {:auto, list} form.
    Enum.map(nifs, &Nif.normalize_options/1)
  end

  def normalize_nifs_option!(auto_list = {:auto, _}, true) do
    # the auto form is not allowed to be used for easy_c nif modules.
    raise CompileError, description: "The {:auto, list} form is not allowed to be used for easy_c nifs"
  end

  def normalize_nifs_option!(list, _) do
    # first check to see if the `...` form is used in the options list, which
    # is an Elixir-only option instead of {:auto, []}

    case transform_elixir_auto(list, []) do
      {:auto, nifs} -> {:auto, Enum.map(nifs, &Nif.normalize_options/1)}
      list -> Enum.map(list, &Nif.normalize_options/1)
    end
  end

  defp transform_elixir_auto([{:..., _, _} | rest], so_far) do
    {:auto, Enum.reverse(rest, so_far)}
  end

  defp transform_elixir_auto([head | rest], so_far) do
    transform_elixir_auto(rest, [head | so_far])
  end

  defp transform_elixir_auto([], so_far), do: Enum.reverse(so_far)
end
