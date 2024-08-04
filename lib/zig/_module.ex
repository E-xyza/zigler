defmodule Zig.Module do
  @moduledoc false
  # abstraction representing multiple zig nif functions bound into a single
  # module

  alias Zig.C
  alias Zig.Nif
  alias Zig.Resources
  alias Zig.Type.Error

  # you'll never see me ever import any other module.
  import Zig.QuoteErl

  # for easy access in EEx files
  @behaviour Access

  @impl true
  defdelegate fetch(function, key), to: Map

  @enforce_keys [:otp_app, :module, :file, :line]

  defstruct @enforce_keys ++
              [
                :on_load,
                :upgrade,
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
                :c,
                :dir,
                :easy_c,
                language: Elixir,
                nifs: {:auto, []},
                ignore: [],
                packages: [],
                resources: [],
                dump: false,
                dump_sema: false,
                dump_build_zig: false,
                callbacks: [],
                default_nif_opts: [],
                external_resources: [],
                attributes: []
              ]

  @type t :: %__MODULE__{
          otp_app: atom(),
          module: module(),
          file: Path.t(),
          line: non_neg_integer(),
          on_load: atom(),
          upgrade: atom(),
          extern: nil | Path.t(),
          module_code_path: nil | Path.t(),
          zig_code_path: nil | Path.t(),
          manifest: nil | Manifest.t(),
          manifest_module: nil | module(),
          sema: Sema.t(),
          parsed: Parser.t(),
          version: String.t(),
          c: C.opts() | C.t(),
          dir: Path.t(),
          easy_c: Path.t(),
          language: Elixir | :erlang,
          nifs: {:auto, Nif.opts()} | Nif.opts() | [Nif.t()],
          ignore: [atom()],
          packages: [packagespec()],
          resources: [atom()],
          dump: boolean,
          dump_sema: boolean,
          dump_build_zig: boolean | :stdout | :stderr | Path.t(),
          build_zig: nil | Path.t(),
          precompiled: nil | precompiledspec(),
          callbacks: callback_opts(),
          default_nif_opts: [Nif.defaultable_opts()],
          attributes: keyword
        }

  @type packagespec() :: {name :: atom(), {path :: Path.t(), deps :: [atom]}}
  # NB: this is going to become more complex for security reasons.
  @type precompiledspec() :: Path.t()
  @type callback_opts() :: [on_load: atom(), on_upgrade: atom(), on_unload: atom()]

  @defaultable_nif_opts ~w[cleanup leak_check]a

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
    |> Keyword.drop(@defaultable_nif_opts)
    |> Keyword.merge(
      default_nif_opts: Keyword.take(opts, @defaultable_nif_opts),
      module: caller.module,
      file: caller.file,
      line: caller.line
    )
    |> normalize_options()
    |> then(&struct!(__MODULE__, &1))
  end

  defp normalize_options(opts) do
    opts
    |> obtain_version
    |> Keyword.update(:c, %C{}, &C.new(&1, Keyword.fetch!(opts, :file)))
    |> Keyword.update(:callbacks, [], &normalize_callbacks(&1, opts))
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

  @callbacks ~w[on_load on_upgrade on_unload]a

  defp normalize_callbacks(callbacks, opts) do
    Enum.map(callbacks, fn
      callback when callback in @callbacks ->
        {callback, callback}

      {callback, _} = option when callback in @callbacks ->
        option

      other ->
        raise CompileError,
          description: "invalid option for callbacks: #{inspect(other)}",
          file: opts[:file],
          line: opts[:line]
    end)
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

  require EEx

  nif = Path.join(__DIR__, "templates/module.zig.eex")
  EEx.function_from_file(:def, :render_zig, nif, [:assigns])

  on_load = Path.join(__DIR__, "templates/on_load.zig.eex")
  EEx.function_from_file(:def, :render_on_load, on_load, [:assigns])

  on_upgrade = Path.join(__DIR__, "templates/on_upgrade.zig.eex")
  EEx.function_from_file(:def, :render_on_upgrade, on_upgrade, [:assigns])

  on_unload = Path.join(__DIR__, "templates/on_unload.zig.eex")
  EEx.function_from_file(:def, :render_on_unload, on_unload, [:assigns])

  def render_elixir(module, zig_code) do
    module_name = "#{module.module}"

    on_load_code =
      if {:__on_load__, 0} in Module.definitions_in(module.module) do
        quote do
          __on_load__()
        end
      else
        0
      end

    external_resources =
      Enum.map(module.external_resources, fn file ->
        quote do
          @external_resource unquote(file)
        end
      end)

    load_nif_fn =
      quote do
        unquote_splicing(external_resources)

        def __load_nifs__ do
          # LOADS the nifs from :code.lib_dir() <> "ebin", which is
          # a path that has files correctly moved in to release packages.
          require Logger

          unquote(module.otp_app)
          |> :code.priv_dir()
          |> Path.join("lib")
          |> Path.join(unquote(module_name))
          |> String.to_charlist()
          |> :erlang.load_nif(unquote(on_load_code))
          |> case do
            :ok ->
              Logger.debug("loaded module at #{unquote(module_name)}")

            error = {:error, any} ->
              Logger.error("loading module #{unquote(module_name)} #{inspect(any)}")
          end
        end
      end

    function_code = Enum.map(module.nifs, &Nif.render_elixir/1)

    # TODO: there might be a smarter way of getting this.
    manifest_code =
      if Enum.any?(module.nifs, &match?(%Error{}, &1.return.type)) do
        quote do
          require Zig.Manifest
          Zig.Manifest.resolver(unquote(module.manifest), unquote(module.zig_code_path), :defp)
        end
      end

    quote do
      # these two attribs can be persisted for code inspection.
      @zigler_module unquote(Macro.escape(module))
      @zig_code unquote(zig_code)

      unquote_splicing(function_code)
      unquote(load_nif_fn)
      unquote(manifest_code)

      def _format_error(_, [{_, _, _, opts} | _rest] = _stacktrace) do
        if formatted = opts[:zigler_error], do: formatted, else: %{}
      end
    end
  end

  def render_erlang(module, _zig_code) do
    otp_app = module.otp_app
    module_name = Atom.to_charlist(module.module)

    init_function =
      quote_erl(
        """
        '__init__'() ->
          erlang:load_nif(filename:join(code:priv_dir(unquote(otp_app)), unquote(module_id)), []).
        """,
        otp_app: otp_app,
        module_id: ~C'lib/' ++ module_name
      )

    function_code = Enum.map(module.nifs, &Nif.render_erlang/1)

    Enum.flat_map(function_code, & &1) ++ init_function
  end

  # Access behaviour guards
  @impl true
  def get_and_update(_, _, _), do: raise("you should not update a module")

  @impl true
  def pop(_, _), do: raise("you should not pop a module")
end
