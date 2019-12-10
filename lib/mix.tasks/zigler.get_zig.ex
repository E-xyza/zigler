defmodule Mix.Tasks.Zigler.GetZig do

  use Mix.Task

  @moduledoc """
  gets a zig version and puts it into the zigler directory under
  the subdirectory zig.

  ## usage

  `mix zigler.get_zig <version>`

  version can be "latest"
  """

  @shortdoc "caches a version of zig."

  require Logger
  require Zigler

  alias Zigler.Compiler

  @zig_dir_path Path.expand("../../zig", Path.dirname(__ENV__.file))

  @impl true
  def run(["latest"]) do
    run([latest_version()])
  end
  def run([version]) do

    # make sure that we're in the correct operating system.
    if match?({:win32, _}, :os.type()) do
      Mix.raise("""
      non-unix systems not currently supported.

      If you think you can help, please help out!  There's no
      reason why zigler can't run on MacOS, butI have no
      way of testing anything MacOS related.

      https://github.com/ityonemo/zigler/issues/15
      """)
    end

    Logger.configure(level: :info)
    Application.ensure_all_started(:mojito)

    tarfile = Compiler.basename(version) <> ".tar.xz"
    # make sure the zig directory path exists and is ready.
    File.mkdir_p!(@zig_dir_path)
    zig_download_path = Path.join(@zig_dir_path, tarfile)

    unless File.exists?(zig_download_path) do
      Logger.info("downloading zig version #{version} and caching in #{@zig_dir_path}.")
      download_location = "https://ziglang.org/download/#{version}/#{tarfile}"

      download_zig_tarball(zig_download_path, download_location)
    end

    # untar the zig directory.
    zig_version_cache = Path.join(@zig_dir_path, "zig-linux-x86_64-#{version}")
    unless File.dir?(zig_version_cache) do
      System.cmd("tar", ["xvf", tarfile], cd: @zig_dir_path)
    end
  end

  def download_zig_tarball(zig_download_path, download_location) do
    Application.ensure_all_started(:ssl)
    case Mojito.get(download_location, [], pool: false, timeout: 100_000) do
      {:ok, download = %{status_code: 200}} ->
        File.write!(zig_download_path, download.body)
      _ -> Mix.raise("failed to download the appropriate zig binary.")
    end
  end

  @doc """
  queries the zig website to obtain the latest version of zig.  Only performed at compile time.
  """
  def latest_version() do
    Application.ensure_all_started(:ssl)
    # find the latest version by querying the download index
    case Mojito.get("https://ziglang.org/download/index.json",[], pool: false, timeout: 100_000) do
      {:ok, %{status_code: 200, body: json}} ->
        json
        |> Jason.decode!
        |> Map.keys
        |> Enum.reject(&(&1 == "master"))
        |> Enum.map(&String.split(&1, "."))
        |> Enum.map(&List.to_tuple/1)
        |> Enum.sort
        |> List.last
        |> Tuple.to_list
        |> Enum.join(".")
      _ -> Mix.raise("failed to ascertain the latest version of zig.")
    end
  end

end
