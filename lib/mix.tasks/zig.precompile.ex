defmodule Mix.Tasks.Zig.Precompile do
  use Mix.Task

  require Logger

  @moduledoc """
  precompiles a group of zig modules for all supported platforms.

  In order to cross-compile windows-msvc, you will need to have the following environment
  variables set:

  - `MSVC_ROOT`: path to the root of a Visual Studio installation
  - `WINSDK_ROOT`: path to the root of a Windows SDK installation
  - `WINSDK_VERSION`: version of the Windows SDK to use, e.g. `10.0.22621.0`

  TODO: add a way to specify subsets of platforms.
  """

  @shortdoc "precompiles a zig module"

  @msvc_dir System.get_env("MSVC_ROOT", "")
  @winsdk_dir System.get_env("WINSDK_ROOT", "")
  @winsdk_version System.get_env("WINSDK_VER", "")

  if Enum.all?([@msvc_dir, @winsdk_dir, @winsdk_version], &(&1 != "")) or
       match?({_, :nt}, :os.type()) do
    @windows [:msvc, :gnu]
  else
    Logger.warning("not cross-compiling windows-msvc")
    @windows [:gnu]
  end

  @triples [
    aarch64: [freebsd: :none, linux: [:gnu, :musl], macos: :none],
    arm: [linux: [:gnueabi, :gnueabihf, :musleabi, :musleabihf]],
    x86_64: [freebsd: :none, linux: [:gnu, :musl], macos: :none, windows: @windows],
    x86: [linux: [:gnu, :musl], windows: :gnu]
  ]

  def run([file]) do
    shas =
      for {arch, targets} <- @triples, reduce: [] do
        acc ->
          for {os, platforms} <- targets, reduce: acc do
            acc2 ->
              for platform <- List.wrap(platforms), reduce: acc2 do
                acc3 -> [compile(file, arch, os, platform) | acc3]
              end
          end
      end
      |> inspect(pretty: true)
      |> String.split("\n")
      |> Enum.join("\n  ")

    IO.puts("""
    Precompilation complete.  Add the following keyword list to the shasum parameters to in web downloads,
    e.g.:

    ```
    @shasum #{shas}

    use Zig, precompiled: {:web, "https://host-path-to-file/Elixir.MyModule.#VERSION.#TRIPLE.#EXT", @shasum}}
    ```
    """)
  end

  defp compile(file, arch, os, platform) do
    this = self()
    callback = fn file -> send(this, {:result, file}) end

    Application.put_env(:zigler, :precompiling, {arch, os, platform, callback})
    [{module, _}] = Code.compile_file(file)

    try do
      receive do
        {:result, file} ->
          file
          |> File.read!()
          |> then(&:crypto.hash(:sha256, &1))
          |> Base.encode16(case: :lower)
          |> then(&{:"#{arch}-#{os}-#{platform}", &1})
      end
    after
      # clean up the build directory
      module
      |> Zig.Builder.staging_directory()
      |> File.rm_rf!()

      Process.sleep(100)
    end
  end
end
