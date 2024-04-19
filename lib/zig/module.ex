defmodule Zig.Module do
  @moduledoc """
  abstraction representing multiple zig nif functions bound into a single
  module
  """

  # TODO: write out a struct that will hold the nif opts for the module.

  alias Zig.Nif
  alias Zig.Resources

  # for easy access in EEx files
  @behaviour Access

  @impl true
  defdelegate fetch(function, key), to: Map

  @enforce_keys [:otp_app, :module, :file]

  defstruct @enforce_keys ++
              [
                :on_load,
                :upgrade,
                :easy_c,
                :extern,
                :build_zig,
                :precompiled,
                :module_code_path,
                :zig_code_path,
                :manifest,
                :manifest_module,
                :sema,
                :parsed,
                :version,
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
                default_nif_opts: []
              ]

  @type t :: %__MODULE__{
          otp_app: atom(),
          module: atom(),
          file: Path.t(),
          on_load: atom(),
          upgrade: atom(),
          easy_c: nil | Path.t(),
          extern: nil | Path.t(),
          module_code_path: nil | Path.t(),
          zig_code_path: nil | Path.t(),
          manifest: nil | Manifest.t(),
          manifest_module: nil | module(),
          sema: Sema.t(),
          parsed: Parser.t(),
          version: String.t(),
          language: Elixir | :erlang,
          nifs: nif_opts() | [Nif.t()],
          ignore: [atom()],
          packages: [packagespec()],
          resources: [atom()],
          dump: boolean,
          dump_sema: boolean,
          dump_build_zig: boolean | :stdout | :stderr | Path.t(),
          build_zig: nil | Path.t(),
          precompiled: nil | precompiledspec(),
          include_dir: [],
          link_lib: [],
          link_libcpp: false,
          c_src: [],
          callbacks: callback_opts(),
          default_nif_opts: default_nif_opts()
        }

  @type packagespec() :: {name :: atom(), {path :: Path.t(), deps :: [atom]}}
  # NB: this is going to become more complex for security reasons.
  @type precompiledspec() :: Path.t()
  @type callback_opts() :: [on_load: atom(), on_upgrade: atom(), on_unload: atom()]
  @type nif_opts() :: [
          cleanup: boolean,
          leak_check: boolean,
          ignore: boolean,
          export: boolean,
          concurrency: Zig.Nif.Concurrency.concurrency(),
          raw: boolean(),
          args: %{optional(integer) => arg_opts()},
          return: :list | :binary,
          alias: atom(),
          doc: String.t(),
          spec: Macro.t()
        ]
  @type arg_opts() :: [{atom, term}]

  @type default_nif_opts() :: [cleanup: boolean, leak_check: boolean]

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
      default_nif_opts: Keyword.take(opts, @nif_opts),
      module: caller.module,
      file: caller.file
    )
    |> normalize_options()
    |> then(&struct!(__MODULE__, &1))
  end

  defp normalize_options(opts) do
    opts
    |> obtain_version

    # |> normalize_nifs
    # |> normalize_libs
    # |> normalize_build_opts
    # |> normalize_include_dirs
    # |> normalize_c_src
    # |> EasyC.normalize_aliasing()
  end

  defp obtain_version(opts) do
    otp_app = Keyword.fetch!(opts, :otp_app)

    Keyword.put_new_lazy(opts, :version, fn ->
      cond do
        # try checking the mix project first (this is if the project is being compiled for the first time)
        function_exported?(Mix.Project, :config, 0) ->
          Mix.Project.config()
          |> Keyword.fetch!(:version)
          |> Version.parse!()

        # try checking the application version (this is if we are hot-patching the so file)
        tuple = Application.loaded_applications()[otp_app] ->
          tuple
          |> elem(2)
          |> Version.parse!()

        :else ->
          Version.parse!("0.0.0")
      end
    end)
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

  require EEx

  nif = Path.join(__DIR__, "templates/module.zig.eex")
  EEx.function_from_file(:def, :render_zig, nif, [:assigns])

  def render_elixir(module, zig_code) do
    nif_name = "#{module.module}"

    load_nif_fn =
      quote do
        def __load_nifs__ do
          # LOADS the nifs from :code.lib_dir() <> "ebin", which is
          # a path that has files correctly moved in to release packages.
          require Logger

          unquote(module.otp_app)
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

    function_code = Enum.map(module.nifs, &Nif.render_elixir/1)

    quote do
      @zig_code unquote(zig_code)
      unquote_splicing(function_code)
      unquote(load_nif_fn)
      require Zig.Manifest
      Zig.Manifest.resolver(unquote(module.manifest), unquote(module.module_code_path), :defp)

      def _format_error(_, [{_, _, _, opts} | _rest] = _stacktrace) do
        if formatted = opts[:zigler_error], do: formatted, else: %{}
      end
    end
  end

  def render_erlang(module, zig_code) do
    raise "unimplemented"
    #  otp_app = Keyword.fetch!(opts, :otp_app)
    #  module_name = Atom.to_charlist(module)
    #
    #  init_function =
    #    quote_erl(
    #      """
    #      '__init__'() ->
    #        erlang:load_nif(filename:join(code:priv_dir(unquote(otp_app)), unquote(module_id)), []).
    #      """,
    #      otp_app: otp_app,
    #      module_id: ~C'lib/' ++ module_name
    #    )
    #
    #  Enum.flat_map(function_code, & &1) ++ init_function
  end

  # Access behaviour guards
  @impl true
  def get_and_update(_, _, _), do: raise("you should not update a module")

  @impl true
  def pop(_, _), do: raise("you should not pop a module")
end
