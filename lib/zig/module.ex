defmodule Zig.Module do
  @moduledoc """
  abstraction representing multiple zig nif functions bound into a single
  module
  """

  # TODO: write out a struct that will hold the nif opts for the module.

  alias Zig.Nif
  alias Zig.Resources

  @enforce_keys [:otp_app, :module, :file]

  defstruct @enforce_keys ++
              [
                :on_load,
                :upgrade,
                :easy_c,
                :extern,
                :build_zig,
                :precompiled,
                :nif_code_path,
                :manifest,
                :manifest_module,
                language: Elixir,
                nifs: [],
                ignore: [],
                packages: [],
                resources: [],
                dump: false,
                dump_sema: false,
                dump_build_zig: false,
                include_dir: [],
                link_lib: [],
                link_libcpp: false,
                c_src: [],
                callbacks: [],
                nif_opts: []
              ]

  @type t :: %__MODULE__{
          otp_app: atom(),
          module: atom(),
          file: Path.t(),
          on_load: atom(),
          upgrade: atom(),
          easy_c: nil | Path.t(),
          extern: nil | Path.t(),
          nif_code_path: nil | Path.t(),
          manifest: nil | Manifest.t(),
          manifest_module: nil | module(),
          language: Elixir | :erlang,
          nifs: [Nif.t()],
          ignore: [atom()],
          packages: [packagespec()],
          resources: [atom()],
          dump: boolean,
          dump_sema: boolean,
          dump_build_zig: boolean | :stdout | :stderr | Path.t,
          build_zig: nil | Path.t(),
          precompiled: nil | precompiledspec(),
          include_dir: [],
          link_lib: [],
          link_libcpp: false,
          c_src: [],
          callbacks: callback_opts(),
          nif_opts: global_nif_opts()
        }

  @type packagespec() :: {name :: atom(), {path :: Path.t(), deps :: [atom]}}
  # NB: this is going to become more complex for security reasons.
  @type precompiledspec() :: Path.t()
  @type callback_opts() :: [on_load: atom(), on_upgrade: atom(), on_unload: atom()]
  @type global_nif_opts() :: [cleanup: boolean, leak_check: boolean, ignore: boolean]

  @nif_opts ~w[cleanup leak_check ignore]a

  def new(opts, caller) do
    # make sure that the caller has declared otp_app here.
    case {Keyword.fetch(opts, :language), Keyword.fetch(opts, :otp_app)} do
      {{:ok, :elixir}, :error} ->
        raise CompileError,
          description:
            "(module #{inspect(caller.module)}) you must supply an `otp_app` option to `use Zig`",
          file: caller.file

      {{:ok, :erlang}, :error} ->
        raise CompileError,
          description:
            "(module #{inspect(caller.module)}) you must supply an `otp_app` option to `zig_opts()`",
          file: caller.file

      _ ->
        :ok
    end

    opts
    |> Keyword.drop(@nif_opts)
    |> Keyword.merge(
      nif_opts: Keyword.take(opts, @nif_opts),
      module: caller.module,
      file: caller.file
    )
    |> normalize_options()
    |> then(&struct!(__MODULE__, &1))
  end

  defp normalize_options(opts) do
    opts
    # |> normalize_nifs
    # |> normalize_libs
    # |> normalize_build_opts
    # |> normalize_include_dirs
    # |> normalize_c_src
    # |> EasyC.normalize_aliasing()
  end

  # defp normalize_nifs(opts) do
  #  Keyword.update!(opts, :nifs, fn
  #    {:auto, opts} ->
  #      {:auto, Enum.map(opts, &Nif.normalize_options!(&1, common_options))}

  #    opts ->
  #      Enum.map(opts, &Nif.normalize_options!(&1, common_options))
  #  end)
  # end

  import Zig.QuoteErl

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
              |> :code.priv_dir()
              |> Path.join("lib")
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
end
