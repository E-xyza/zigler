use Protoss

defprotocol Zig.Builder do
  @moduledoc false

  # Code for interfacing with `std.build.Builder`, the interface for programmatically invoking
  # build code with the `zig build` command.

  @spec render(t) :: iodata()
  def render(assigns, opts)
after
  require EEx
  require Logger
  alias Zig.Attributes
  alias Zig.Command

  defmacro __using__(opts) do
    template = Keyword.fetch!(opts, :template)

    quote do
      defdelegate fetch(struct, key), to: Map

      require EEx
      render = Path.join(__DIR__, unquote(template))
      EEx.function_from_file(:def, :render, render, [:assigns, :opts])
      defoverridable render: 2
    end
  end

  def render(assigns), do: render(assigns, [])

  def staging_directory(module) do
    staging_root =
      case System.get_env("ZIGLER_STAGING_ROOT", "") do
        "" -> System.tmp_dir()
        path -> path
      end

    Path.join(staging_root, to_string(module))
  end

  def beam_file(path) do
    :zigler
    |> :code.priv_dir()
    |> Path.join("beam/#{path}")
  end

  def stage(module) do
    staging_directory = staging_directory(module.module)

    unless File.dir?(staging_directory) do
      Logger.debug("creating staging directory #{staging_directory}")

      File.mkdir_p!(staging_directory)
    end

    # assigns = %{
    #  module: module.module,
    #  version: module.version,
    #  beam_dir: Path.join(:code.priv_dir(:zigler), "beam"),
    #  c: module.c,
    #  packages: make_modules(module),
    #  zig_code_path: module.zig_code_path,
    #  modules: []
    # }
    #
    # assigns =
    #  case :os.type() do
    #    {_, :nt} ->
    #      :zigler
    #      |> :code.priv_dir()
    #      |> Path.join("erl_nif_win")
    #      |> then(&Map.put(assigns, :windows_shim_dir, &1))
    #
    #    _ ->
    #      assigns
    #  end
    #
    # build_file = build_zig(assigns)
    #

    # TODO: move to Attributes module.
    attribs_path = Path.join(staging_directory, "attributes.zig")
    File.write!(attribs_path, Enum.map(module.attributes, &Attributes.render_zig/1))

    build_zig_path = Path.join(staging_directory, "build.zig")

    File.write!(build_zig_path, render(module))
    Command.fmt(build_zig_path)

    Logger.debug("wrote build.zig to #{build_zig_path}")

    %{module | module_code_path: Path.join(staging_directory, "module.zig")}
  rescue
    e in File.Error ->
      new_action = "#{e.action}, consider setting ZIGLER_STAGING_ROOT environment variable\n"
      reraise %{e | action: new_action}, __STACKTRACE__
  end
end
