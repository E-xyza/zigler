defmodule Mix.Tasks.Zig.Get do
  use Mix.Task

  @shortdoc "Obtains the Zig compiler toolchain"

  @moduledoc """
  obtains the Zig compiler toolchain

  It expects the path of the project as an argument.

      $ mix zig.get [--version VERSION] [--from FROM] [--path PATH] [--os OS] [--arch ARCH]

  the zigler compiler will be downloaded to PATH/VERSION

  if unspecified, VERSION defaults to the major/minor version of zig.get

  if FROM is specified, will use the FROM file instead of getting from the internet.

  if unspecified, PATH defaults to the user cache path given by
  `:filename.basedir/3` with application name `"zigler"`.

  OS and ARCH will be detected from the current build system.  It's not
  recommended to change these arguments.
  """

  defstruct ~w(version path arch os url file)a

  def run(app_opts) do
    :ssl.cipher_suites(:all, :"tlsv1.2")
    :application.ensure_all_started(:inets)

    opts = parse_opts(app_opts)

    opts
    |> ensure_destination
    |> url_for
    |> request
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

  defp defaults do
    {os, arch} = os_info()

    %__MODULE__{
      version: default_version(),
      path: :filename.basedir(:user_cache, ~C"zigler"),
      os: os,
      arch: arch
    }
  end

  defp default_version do
    :application.info()
    |> Keyword.fetch!(:loaded)
    |> List.keyfind(:zig_get, 0)
    |> elem(2)
    |> to_string()
  end

  defp os_info do
    :system_architecture
    |> :erlang.system_info()
    |> to_string
    |> String.split("-")
    |> decode_os_info()
  end

  defp decode_os_info([arch, _vendor, os | _]), do: {os, arch}

  defp ensure_destination(opts) do
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

  defp request(%{file: file}) do
    IO.puts("Obtaining Zig compiler toolchain from #{file}")
    File.read!(file)
  end

  defp request(%{url: url}) do
    spin_with("Downloading Zig compiler toolchain from #{url} ", fn ->
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
    # performs gzip inflate first, then tar.
    {:spawn_executable, System.find_executable("tar")}
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

  @spinners ~w(| / - \\)
  defp spinner, do: spinner(@spinners)

  defp spinner([head | tail]) do
    IO.write([head, IO.ANSI.cursor_left(1)])
    Process.sleep(500)
    spinner(tail)
  end

  defp spinner([]), do: spinner(@spinners)
end
