defmodule Zig.Module do
  @moduledoc """
  abstraction representing multiple zig nif functions bound into a single
  module
  """

  alias Zig.Nif
  alias Zig.Resources

  @doc """
  obtains a list of Nif structs from the semantically analyzed content and
  the nif options that are a part of
  """
  # TODO: unit test this function directly.
  def nifs_from_sema(sema_list, nif_opts) do
    Enum.map(sema_list, fn
      {name, function} ->
        Nif.new(function, Keyword.fetch!(nif_opts, name))
    end)
  end

  # TODO: move this to Module, since this is module-common stuff.
  require EEx

  nif = Path.join(__DIR__, "templates/module.zig.eex")
  EEx.function_from_file(:def, :module_file, nif, [:assigns])

  def render_zig(nifs, resources, callbacks, module) do
    resources = append_concurrency_resources(resources, nifs)
    module_file(binding())
  end

  defp append_concurrency_resources(resources, nifs) do
    nifs
    |> Enum.flat_map(& &1.concurrency.resources(&1))
    |> Kernel.++(resources)
  end

  # internal helpers
  defp table_entries(nifs) when is_list(nifs) do
    nifs
    |> Enum.flat_map(&Nif.table_entries/1)
    |> Enum.join(",")
  end

  @index_of %{major: 0, minor: 1}

  defp nif_version(at) do
    :nif_version
    |> :erlang.system_info()
    |> List.to_string()
    |> String.split(".")
    |> Enum.at(@index_of[at])
  end

  #############################################################################
  ## OPTIONS NORMALIZATION

  @doc false
  def normalize_nifs_option!(:auto, _, nil), do: {:auto, []}

  def normalize_nifs_option!({:auto, nifs}, common_options, nil) do
    # for erlang: we can specify that we want to automatically find functions,
    # but override the list of functions provided using the {:auto, list} form.
    Enum.map(nifs, &Nif.normalize_options!(&1, common_options))
  end

  def normalize_nifs_option!({:auto, _}, _, _) do
    # the auto form is not allowed to be used for easy_c nif modules.
    raise CompileError,
      description: "The `{:auto, list}` form is not allowed to be used for easy_c nifs"
  end

  def normalize_nifs_option!(list, common_options, _) do
    # first check to see if the `...` form is used in the options list, which
    # is an Elixir-only option instead of {:auto, []}

    case transform_elixir_auto(list, []) do
      {:auto, nifs} -> {:auto, Enum.map(nifs, &Nif.normalize_options!(&1, common_options))}
      list -> Enum.map(list, &Nif.normalize_options!(&1, common_options))
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
