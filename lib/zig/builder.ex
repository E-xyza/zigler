use Protoss

defprotocol Zig.Builder do
  @moduledoc false

  # Code for interfacing with `std.build.Builder`, the interface for programmatically invoking
  # build code with the `zig build` command.

  @spec render_build(t) :: iodata()
  def render_build(assigns, opts)
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
      render_template = Path.join(__DIR__, unquote(template))
      EEx.function_from_file(:def, :render_build, render_template, [:assigns, :opts])
      defoverridable render_build: 2
    end
  end

  def render_build(assigns), do: render_build(assigns, [])

  def staging_directory(module) do
    staging_root =
      case System.get_env("ZIGLER_STAGING_ROOT", "") do
        "" -> System.tmp_dir()
        path -> path
      end

    Path.join(staging_root, to_string(module))
  end

  # this is required because Elixir version < 1.16 doesn't support Path.relative_to/3
  def staging_directory(module, from) do
    case {staging_directory(module), from} do
      {"/" <> mod_rest, "/" <> from_rest} ->
        from_rest
        |> String.split("/")
        |> force_relative(String.split(mod_rest, "/"))

      {dir_mod, _} ->
        Path.relative_to(from, dir_mod)
    end
  end

  defp force_relative([same | rest_left], [same | rest_right]),
    do: force_relative(rest_left, rest_right)

  defp force_relative([], []), do: "."

  defp force_relative(left, others) do
    others
    |> length()
    |> then(&List.duplicate("..", &1))
    |> Path.join()
    |> Path.join(Path.join(left))
  end

  EEx.function_from_file(
    :def,
    :build_zig_zon,
    Path.join(__DIR__, "templates/build.zig.zon.eex"),
    [:assigns]
  )

  def beam_file(path) do
    :zigler
    |> :code.priv_dir()
    |> Path.join("beam/#{path}")
  end

  def stage(module = %{precompiled: nil}) do
    staging_directory = staging_directory(module.module)

    unless File.dir?(staging_directory) do
      Logger.debug("creating staging directory #{staging_directory}")

      File.mkdir_p!(staging_directory)
    end

    # TODO: move to Attributes module.
    attribs_path = Path.join(staging_directory, "attributes.zig")
    File.write!(attribs_path, Enum.map(module.attributes, &Attributes.render_zig/1))

    build_zig_path = Path.join(staging_directory, "build.zig")
    build_zig_zon_path = Path.join(staging_directory, "build.zig.zon")

    if dir = module.build_files_dir do
      dir
      |> Zig._normalize_path(Path.dirname(module.file))
      |> Path.join("build.zig")
      |> File.cp!(build_zig_path)

      dir
      |> Zig._normalize_path(Path.dirname(module.file))
      |> Path.join("build.zig.zon")
      |> File.cp!(build_zig_zon_path)
    else
      File.write!(build_zig_path, render_build(module))
      Command.fmt(build_zig_path)

      File.write!(build_zig_zon_path, build_zig_zon(module))
    end

    Logger.debug("wrote build.zig to #{build_zig_path}")

    %{module | module_code_path: Path.join(staging_directory, "module.zig")}
  rescue
    e in File.Error ->
      new_action = "#{e.action}, consider setting ZIGLER_STAGING_ROOT environment variable\n"
      reraise %{e | action: new_action}, __STACKTRACE__
  end

  # if precompiled file is specified, do nothing.
  def stage(module), do: module
end
