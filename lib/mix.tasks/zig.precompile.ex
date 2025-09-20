defmodule Mix.Tasks.Zig.Precompile do
  use Mix.Task

  @shortdoc "precompiles a zig module"

  @triples [
    aarch64: [freebsd: :none, linux: [:gnu, :musl], macos: :none],
    arm: [linux: [:gnueabi, :gnueabihf, :musleabi, :musleabihf]],
    x86_64: [freebsd: :none, linux: [:gnu, :musl], macos: :none, windows: :gnu],
    x86: [linux: [:gnu, :musl], windows: :gnu]
  ]

  def run([file]) do
    shas =
      for {arch, targets} <- @triples, reduce: [] do
        acc ->
          for {os, platforms} <- targets, reduce: acc do
            acc2 ->
              for platform <- List.wrap(platforms), into: acc2 do
                compile(file, arch, os, platform)
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
