for path <- :code.get_path(),
    Regex.match?(~r/zig_get-[\w\.\-]+\/ebin$/, List.to_string(path)) do
  Code.delete_path(path)
end

defmodule Zig.Get.MixProject do
  use Mix.Project

  @version "0.15.2"
  @scm_url "https://github.com/e-xyza/zigler"

  @elixir_requirement "~> 1.14"

  def project do
    [
      app: :zig_get,
      start_permanent: Mix.env() == :prod,
      version: @version,
      elixir: @elixir_requirement,
      deps: deps(),
      package: [
        maintainers: ["Isaac Yonemoto"],
        licenses: ["MIT"],
        links: %{"GitHub" => @scm_url},
        files: ~w[mix.tasks mix.exs README.md]
      ],
      preferred_cli_env: [docs: :docs],
      source_url: @scm_url,
      docs: docs(),
      elixirc_paths: elixirc_paths(),
      description: """
      `zig.get` is a Mix task that downloads and installs the Zig compiler toolchain.
      """
    ]
  end

  def application do
    [
      extra_applications: [:eex, :crypto, :inets, :public_key, :ssl]
    ]
  end

  defp json do
    case Code.ensure_loaded(:json) do
      {:module, :json} ->
        []

      _ ->
        [{:jason, "~> 1.4", runtime: Mix.env() == :test}]
    end
  end

  def deps do
    [
      {:ex_doc, "~> 0.24", only: :dev}
    ] ++ json()
  end

  defp elixirc_paths, do: ~w[mix.tasks]

  defp docs do
    [
      source_url_pattern: "#{@scm_url}/blob/main/installer/%{path}#L%{line}"
    ]
  end
end
