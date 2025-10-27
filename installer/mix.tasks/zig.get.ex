defmodule Zig.Get do
  @moduledoc false

  def os_info do
    :system_architecture
    |> :erlang.system_info()
    |> to_string
    |> String.split("-")
    |> decode_os_info()
  end

  defp decode_os_info([arch, "apple" | _]), do: {"macos", arch}

  defp decode_os_info([_, "freebsd" | _]) do
    Mix.raise("FreeBSD is not supported by the installer: please install Zig manually")
  end

  defp decode_os_info([arch, _vendor, os | _]), do: {os, arch}
  defp decode_os_info(["win32"]), do: {"windows", "x86_64"}
end

defmodule Mix.Tasks.Zig.Get do
  use Mix.Task

  @shortdoc "Obtains the Zig compiler toolchain"

  @default_version "0.15.2"

  @moduledoc """
  obtains the Zig compiler toolchain

      $ mix zig.get [--version VERSION] [--from FROM] [--os OS] [--arch ARCH] [other options]

  the zigler compiler will be downloaded to ZIG_ARCHIVE_PATH/VERSION

  if unspecified, VERSION defaults to #{@default_version}.

  if FROM is specified, will use the FROM file instead of getting from the internet.

  if unspecified, ZIG_ARCHIVE_PATH defaults to the user cache path given by
  `:filename.basedir/3` with application name `"zigler"`.

  OS and ARCH will be detected from the current build system.  It's not
  recommended to change these arguments.

  ### environment variable options

  - `TAR_COMMAND`: path to a tar executable that is equivalent to gnu tar.
    only useful for non-windows architectures.
  - `ZIG_ARCHIVE_PATH`: path to desired directory to achive the zig compiler toolchain.

  ### other options

  - `--force` overwrites the existing installation if it exists.
  - `--disable-verify` disables the hash verification of the downloaded file.
    it's possible that the manifest at `https://ziglang.org/download/index.json`
  """

  defstruct ~w[version path arch os url file verify hash force]a

  def run(app_opts) do
    # Elixir 1.14 cannot take a list of applications for this function
    Enum.each([:inets, :ssl, :crypto], &Application.ensure_all_started/1)

    :ssl.cipher_suites(:all, :"tlsv1.2")

    app_opts
    |> parse_opts()
    |> set_archive_path()
    |> ensure_tar()
    |> verify()
    |> get_meta()
    |> ensure_destination
    |> request!
    |> verify_hash!
    |> do_extract

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

  defp parse_opts(["--os", os | rest], so_far) do
    parse_opts(rest, %{so_far | os: os})
  end

  defp parse_opts(["--arch", arch | rest], so_far) do
    parse_opts(rest, %{so_far | arch: arch})
  end

  defp parse_opts(["--force" | rest], so_far) do
    parse_opts(rest, %{so_far | force: true})
  end

  defp set_archive_path(opts) do
    case System.get_env("ZIG_ARCHIVE_PATH", "") do
      "" -> opts
      path -> %{opts | path: to_charlist(Path.expand(path))}
    end
  end

  defp defaults do
    {os, arch} = Zig.Get.os_info()

    %__MODULE__{
      version: @default_version,
      path: :filename.basedir(:user_cache, ~C"zigler"),
      os: os,
      arch: arch,
      force: false
    }
  end

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

    cond do
      File.exists?(target_directory) && opts.force ->
        File.rm_rf!(target_directory)

      File.exists?(target_directory) ->
        Mix.shell().info("zig is already installed, rerun with --force to overwrite")
        System.halt()

      true ->
        :nothing_to_do
    end

    File.mkdir_p!(opts.path)

    opts
  end

  defp verify(opts) do
    if System.get_env("VERIFY", "true") == "false" do
      %{opts | verify: false}
    else
      %{opts | verify: true}
    end
  end

  case Code.ensure_loaded(:json) do
    {:module, :json} ->
      defp json_decode!(string), do: :json.decode(string)

    _ ->
      defp json_decode!(string), do: Jason.decode!(string)
  end

  defp get_meta(opts) do
    meta =
      "https://ziglang.org/download/index.json"
      |> http_get!()
      |> json_decode!()
      |> Map.fetch!(opts.version)
      |> Map.fetch!("#{opts.arch}-#{opts.os}")

    hash = if opts.verify, do: Map.fetch!(meta, "shasum")

    %{opts | hash: hash, url: Map.fetch!(meta, "tarball")}
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

  defp verify_hash!({contents, opts} = state) do
    if opts.verify do
      hashed =
        :sha256
        |> :crypto.hash(contents)
        |> Base.encode16(case: :lower)

      unless hashed == opts.hash do
        Mix.raise("hash mismatch: expected #{opts.hash}, got #{hashed}")
      end

      state
    else
      state
    end
  end

  defp do_extract({bin, opts}) do
    spin_with("Extracting Zig compiler toolchain to #{opts.path} ", fn ->
      {:ok, list} = extract_mod(opts).extract(bin, extract_opts(opts))
      if !is_list(list), do: raise("extraction failed")
    end)
  end

  defp extract_mod(%{os: "windows"}), do: :zip
  defp extract_mod(_), do: __MODULE__

  defp extract_opts(%{path: path}) do
    [cwd: path]
  end

  def extract(bin, opts) do
    {:spawn_executable, System.fetch_env!("TAR_COMMAND")}
    |> Port.open(args: ~w[-xJf -], cd: opts[:cwd])
    |> Port.command(bin)

    {:ok, []}
  end

  defp spin_with(message, fun) do
    IO.write(message)
    spinner = spawn(&spinner/0)

    result = fun.()

    IO.write(newline())
    Process.exit(spinner, :normal)
    result
  end

  @otp_version :otp_release
               |> :erlang.system_info()
               |> List.to_integer()

  if @otp_version >= 25 do
    defp ssl_opts do
      [
        verify: :verify_peer,
        cacerts: :public_key.cacerts_get()
      ]
    end
  else
    defp ssl_opts do
      # unfortunately in otp 24 there is not a clean way of obtaining cacerts
      []
    end
  end

  defp http_get!(url) do
    {:ok, {{_, 200, _}, _headers, body}} =
      :httpc.request(
        :get,
        {url, []},
        [
          ssl:
            [
              depth: 100,
              customize_hostname_check: [
                match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
              ]
            ] ++ ssl_opts()
        ],
        body_format: :binary
      )

    body
  end

  @spinners ~w[| / - \\]
  defp spinner, do: spinner(@spinners)

  defp spinner([head | tail]) do
    IO.write([head, IO.ANSI.cursor_left(1)])
    Process.sleep(500)
    spinner(tail)
  end

  defp spinner([]), do: spinner(@spinners)

  defp newline do
    case :os.type() do
      {_, :nt} -> "\r\n"
      _ -> "\n"
    end
  end
end
