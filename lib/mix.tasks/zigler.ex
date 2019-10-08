if Mix.env() == :dev do

defmodule Mix.Tasks.Zigler.GetZig do
  @moduledoc """
  gets a zig version and puts it into the zigler directory under
  the subdirectory zig.

  ## usage

  `mix zigler.get_zig <version>`

  version can be "latest"
  """

  require Logger

  @zig_dir_path Path.expand("../../../zig", __ENV__.file)
  @latest_version Application.get_env(:zigler, :latest_zig_version)

  def run(["latest"]), do: run([@latest_version])
  def run([version]) do

    unless {:unix, :linux} == :os.type() do
      Mix.raise("""
      non-linux systems not currently supported.

      If you think you can help, please help out!  There's no
      reason why zigler can't run on MacOS, butI have no
      way of testing anything MacOS related.

      https://github.com/ityonemo/zigler/issues/15
      """)
    end

    Logger.configure(level: :info)
    Application.ensure_all_started(:mojito)

    tarfile = "zig-linux-x86_64-#{version}.tar.xz"
    zig_download_path = Path.join(@zig_dir_path, tarfile)

    unless File.exists?(zig_download_path) do
      Logger.info("downloading zig version #{version} and caching in #{@zig_dir_path}.")
      download_location = "https://ziglang.org/download/#{version}/#{tarfile}"

      download_zig_tarball(zig_download_path, download_location)
    end

    # TODO: check the shasum.

    # untar the zig directory.
    zig_version_cache = Path.join(@zig_dir_path, "zig-linux-x86_64-#{version}")
    unless File.dir?(zig_version_cache) do
      System.cmd("tar", ["xvf", tarfile], cd: @zig_dir_path)
    end
  end

  def download_zig_tarball(zig_download_path, download_location) do
    case Mojito.get(download_location, [], pool: false, timeout: 100_000) do
      {:ok, download = %{status_code: 200}} ->
        File.write!(zig_download_path, download.body)
      _ -> Mix.raise("failed to download the appropriate zig binary.")
    end
  end

end

end
