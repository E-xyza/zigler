defmodule Zig.Module do
  @moduledoc """
  abstraction representing multiple zig nif functions bound into a single
  module
  """

  alias Zig.Nif
  alias Zig.Resources

  import Zig.QuoteErl

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

  # CODE RENDERING

  def render_elixir(code, function_code, module, manifest, opts) do
    nif_name = "#{module}"

    load_nif_fn =
      case function_code do
        [] ->
          logger_msg = "module #{inspect(module)} does not compile its nifs"

          quote do
            def __load_nifs__ do
              require Logger
              Logger.info(unquote(logger_msg))
            end
          end

        _ ->
          quote do
            def __load_nifs__ do
              # LOADS the nifs from :code.lib_dir() <> "ebin", which is
              # a path that has files correctly moved in to release packages.
              require Logger

              unquote(opts[:otp_app])
              |> :code.lib_dir()
              |> Path.join("ebin/lib")
              |> Path.join(unquote(nif_name))
              |> String.to_charlist()
              |> :erlang.load_nif(0)
              |> case do
                :ok ->
                  Logger.debug("loaded module at #{unquote(nif_name)}")

                error = {:error, any} ->
                  Logger.error("loading module #{unquote(nif_name)} #{inspect(any)}")
              end
            end
          end
      end

    quote do
      @zig_code unquote(code)
      unquote_splicing(function_code)
      unquote(load_nif_fn)
      require Zig.Manifest
      Zig.Manifest.resolver(unquote(manifest))

      def _format_error(_, [{_, _, _, opts} | _rest] = _stacktrace) do
        if formatted = opts[:zigler_error], do: formatted, else: %{}
      end
    end
  end

  def render_erlang(_code, function_code, module, _manifest, opts) do
    otp_app = Keyword.fetch!(opts, :otp_app)
    module_name = Atom.to_charlist(module)

    init_function =
      quote_erl(
        """
        '__init__'() ->
          erlang:load_nif(filename:join(code:priv_dir(unquote(otp_app)), unquote(module_id)), []).
        """,
        otp_app: otp_app,
        module_id: ~C'lib/' ++ module_name
      )

    Enum.flat_map(function_code, & &1) ++ init_function
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
