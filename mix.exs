defmodule Zigler.MixProject do
  use Mix.Project

  def project do
    [
      app: :zigler,
      version: "0.1.2",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      deps: deps(),
      aliases: [docs: "zig_doc"],
      package: [
        description: "Zig nif library",
        licenses: ["MIT"],
        files: ~w(lib mix.exs README* LICENSE* VERSIONS* assets zig/beam zig/include),
        links: %{"GitHub" => "https://github.com/ityonemo/zigler", "Zig" => "https://ziglang.org/"}
      ],
      dialyzer: [plt_add_deps: :transitive],
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        dialyzer: :dev],
      source_url: "https://github.com/ityonemo/zigler/",
      docs: [main: "Zigler", extras: ["README.md"]]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(:dev), do: ["lib", "zigdoc"]
  defp elixirc_paths(:doc), do: ["lib", "zigdoc"]
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  def deps do
    [
      # credo
      {:credo, "~> 1.1.0", only: [:dev, :test], runtime: false},
      # dialyzer
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
      # coverage testing
      {:excoveralls, "~> 0.12", only: :test},
      # zigler's parsing is done using nimble_parsec
      {:nimble_parsec, "~> 0.5", runtime: false},
      # we need this for mix zigler.get_zig mix task.
      {:mojito, "~> 0.6.0", runtime: false},
      # to parse the zig JSON
      {:jason, "~> 1.1", runtime: false},
      # documentation
      {:ex_doc, "~> 0.21", runtime: false},
    ]
  end
end
