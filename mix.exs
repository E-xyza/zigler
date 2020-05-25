defmodule Zigler.MixProject do
  use Mix.Project

  # for the Zigler application, tests are divided into two groups:  Unit tests
  # are all tests which don't require any sort of zig compilation and
  # integration tests are all tests which do.
  #
  # `mix test` will run the full suite of tests.
  # `mix test.unit` will only run the unit tests.

  def project do
    env = Mix.env()
    if Mix.env in [:unit, :isolated] do
      # elixir doesn't allow us to run tests in any environment besides `:test`.
      # so if we send ourselves into the unit tests, we'll have to ninja
      # ourselves back into the test environment.
      Mix.env(:test)
    end

    [
      app: :zigler,
      version: "0.3.0-pre",
      elixir: "~> 1.9",
      start_permanent: env == :prod,
      elixirc_paths: elixirc_paths(env),
      deps: deps(),
      #aliases: [docs: "zig_doc", "test.unit": "test", "test.isolated": "test"],
      aliases: ["test.unit": "test", "test.isolated": "test"],
      package: [
        description: "Zig nif library",
        licenses: ["MIT"],
        # we need to package the zig BEAM adapters and the c include files as a part
        # of the hex packaging system.
        files: ~w(lib mix.exs README* LICENSE* VERSIONS* priv),
        links: %{"GitHub" => "https://github.com/ityonemo/zigler", "Zig" => "https://ziglang.org/"}
      ],
      dialyzer: [plt_add_deps: :transitive],
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        "test.unit": :unit,
        "test.isolated": :isolated,
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
        dialyzer: :dev],
      test_paths: test_paths(env),
      source_url: "https://github.com/ityonemo/zigler/",
      docs: [main: "Zigler", extras: ["README.md"]]
    ]
  end

  def application, do: [extra_applications: [:logger]]

  defp elixirc_paths(:dev), do: ["lib"]
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(:unit), do: ["lib"]
  defp elixirc_paths(:isolated), do: ["lib", "test/isolated/support"]
  defp elixirc_paths(_), do: ["lib"]

  # running `mix test` executes both integration tests and unit tests.
  # running `mix test.unit` executes just the unit tests.
  # comment-twiddle next lines to run a single test in isolation
  defp test_paths(:test), do: ["test/integration", "test/unit"]
  defp test_paths(:unit), do: ["test/unit"]
  defp test_paths(:isolated), do: ["test/isolated"]
  defp test_paths(_), do: []

  def deps do
    [
      # credo
      {:credo, "~> 1.4.0", only: [:dev, :test], runtime: false},
      # dialyzer
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
      # coverage testing
      {:excoveralls, "~> 0.12", only: :test, runtime: false},
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
