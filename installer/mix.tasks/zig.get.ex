defmodule Mix.Tasks.Zig.Get do
  use Mix.Task

  @shortdoc "Obtains the Zig compiler toolchain"

  @moduledoc """
  obtains the Zig compiler toolchain

  It expects the path of the project as an argument.

      $ mix zig.get [--version VERSION] [--from FROM] [--path PATH] [--os OS] [--arch ARCH] [--public-key PUBLIC_KEY]

  the zigler compiler will be downloaded to PATH/VERSION

  if unspecified, VERSION defaults to the major/minor version of zig.get

  if FROM is specified, will use the FROM file instead of getting from the internet.

  if PUBLIC_KEY is not specified, will attempt to get the public key from 
  https://ziglang.org/download/ note that obtaining the public key in this fashion is 
  fragile and provided only for convenience.

  if unspecified, PATH defaults to the user cache path given by
  `:filename.basedir/3` with application name `"zigler"`.

  OS and ARCH will be detected from the current build system.  It's not
  recommended to change these arguments.

  ### environment variable options

  - `TAR_COMMAND`: path to a tar executable that is equivalent to gnu tar.
    only useful for non-windows architectures.
  - `NO_VERIFY`: disable signature verification of the downloaded file.  
    Not recommended. 
  """

  defstruct ~w(version path arch os url file verify public_key signature)a

  def run(app_opts) do
    :ssl.cipher_suites(:all, :"tlsv1.2")
    :application.ensure_all_started(:inets)

    opts =
      app_opts
      |> parse_opts()
      |> ensure_tar()
      |> select_no_verify()
      |> get_public_key()

    opts
    |> ensure_destination
    |> url_for
    |> fetch_signature!
    |> request!
    |> verify_signature!
    |> do_extract(opts)

    IO.puts("completed download of zig compiler toolchain")
  end

  defp parse_opts(app_opts), do: parse_opts(app_opts, defaults())

  defp parse_opts([], so_far), do: so_far

  defp parse_opts(["--version", version | rest], so_far) do
    parse_opts(rest, %{so_far | version: version})
  end

  defp parse_opts(["--file", file | rest], so_far) do
    parse_opts(rest, %{so_far | file: file})
  end

  defp parse_opts(["--path", path | rest], so_far) do
    parse_opts(rest, %{so_far | path: to_charlist(Path.expand(path))})
  end

  defp parse_opts(["--os", os | rest], so_far) do
    parse_opts(rest, %{so_far | os: os})
  end

  defp parse_opts(["--arch", arch | rest], so_far) do
    parse_opts(rest, %{so_far | arch: arch})
  end

  @default_version Zig.Get.MixProject.project()[:version]

  defp defaults do
    {os, arch} = os_info()

    %__MODULE__{
      version: @default_version,
      path: :filename.basedir(:user_cache, ~C"zigler"),
      os: os,
      arch: arch
    }
  end

  defp os_info do
    :system_architecture
    |> :erlang.system_info()
    |> to_string
    |> String.split("-")
    |> decode_os_info()
  end

  defp decode_os_info([arch, _vendor, os | _]), do: {os, arch}

  defp ensure_tar(%{os: "windows"} = opts), do: opts

  defp ensure_tar(opts) do
    cond do
      System.get_env("TAR_COMMAND") ->
        opts

      tar_command = System.find_executable("tar") ->
        System.put_env("TAR_COMMAND", tar_command)
        opts

      true ->
        Mix.raise(
          "tar command is required to install zig on this system architecture but not found.  Please install tar and try again."
        )
    end
  end

  defp ensure_destination(opts) do
    target_directory = Path.join(opts.path, "zig-#{opts.os}-#{opts.arch}-#{opts.version}")

    if File.exists?(target_directory) do
      Mix.raise(
        "destination directory #{target_directory} already exists.  Please remove it and try again."
      )
    end

    File.mkdir_p(opts.path)

    opts
  end

  defp url_for(opts) do
    %{
      opts
      | url:
          ~c"https://ziglang.org/builds/zig-#{opts.os}-#{opts.arch}-#{opts.version}.#{extension(opts)}"
    }
  end

  defp extension(%{os: "windows"}), do: "zip"
  defp extension(_), do: "tar.xz"

  defp select_no_verify(opts) do
    if System.get_env("VERIFY") == "false", do: %{opts | verify: false}, else: opts
  end

  defp get_public_key(%{verify: false} = opts), do: opts
  defp get_public_key(opts) do
    # this might be fragile.
    public_key = http_get!("https://ziglang.org/download/")
    |> Floki.parse_document!()
    |> Floki.find("[role=\"main\"] .container pre code")
    |> Floki.text
    |> String.trim
    %{opts | public_key: public_key}
  end

  defp request!(%{file: file} = opts) when not is_nil(file) do
    IO.puts("Obtaining Zig compiler toolchain from #{file}")
    {File.read!(file), opts}
  end

  defp request!(%{url: url} = opts) do
    spin_with("Downloading Zig compiler toolchain from #{url} ", fn ->
      {http_get!(url), opts}
    end)
  end

  defp do_extract(bin, opts) do
    spin_with("Extracting Zig compiler toolchain to #{opts.path} ", fn ->
      extract_mod(opts).extract({:binary, bin}, extract_opts(opts))
    end)
  end

  defp extract_mod(%{os: "windows"}), do: :zip
  defp extract_mod(_), do: __MODULE__

  defp extract_opts(%{path: path}) do
    [cwd: path]
  end

  def extract({:binary, bin}, opts) do
    {:spawn_executable, System.fetch_env!("TAR_COMMAND")}
    |> Port.open(args: ~w(-xJf -), cd: opts[:cwd])
    |> Port.command(bin)

    :ok
  end

  defp spin_with(message, fun) do
    IO.write(message)
    spinner = spawn(&spinner/0)

    result = fun.()

    IO.write("\n")
    Process.exit(spinner, :normal)
    result
  end

  defp http_get!(url) do
    {:ok, {{_, 200, _}, _headers, body}} =
      :httpc.request(
        :get,
        {url, []},
        [
          ssl: [
            verify: :verify_peer,
            cacerts: :public_key.cacerts_get(),
            depth: 100,
            customize_hostname_check: [
              match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
            ]
          ]
        ],
        body_format: :binary
      )
    body
  end

  defp fetch_signature!(%{verify: false} = opts), do: opts
  defp fetch_signature!(opts) do
    %{opts | signature: http_get!(opts.url ++ ~c'.minisig')}
  end

  defp verify_signature!({bin, %{verify: false}}), do: bin
  defp verify_signature!({bin, opts}) do
    Minisign.verify!(bin, opts.signature, opts.public_key)
    bin
  end

  @spinners ~w(| / - \\)
  defp spinner, do: spinner(@spinners)

  defp spinner([head | tail]) do
    IO.write([head, IO.ANSI.cursor_left(1)])
    Process.sleep(500)
    spinner(tail)
  end

  defp spinner([]), do: spinner(@spinners)
end
