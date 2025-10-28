defmodule Zigler.MixProject do
  use Mix.Project

  def zig_version, do: "0.15.2"

  def project do
    env = Mix.env()

    [
      app: :zigler,
      version: "0.15.2",
      elixir: "~> 1.15",
      start_permanent: env == :prod,
      elixirc_paths: elixirc_paths(env),
      deps: deps(),
      package: [
        description: "Zig nif library",
        licenses: ["MIT"],
        # we need to package the zig BEAM adapters and the c include files as a part
        # of the hex packaging system.
        files: ~w[lib mix.exs README* LICENSE* VERSIONS* priv/beam],
        links: %{
          "GitHub" => "https://github.com/E-xyza/zigler",
          "Zig" => "https://ziglang.org/"
        }
      ],
      dialyzer: [plt_add_deps: :transitive],
      source_url: "https://github.com/E-xyza/zigler/",
      docs: docs(),
      aliases: [
        docs: "zig_doc",
        tidewave:
          "run --no-halt -e 'Agent.start(fn -> Bandit.start_link(plug: Tidewave, port: 4000) end)'"
      ],
      test_elixirc_options: [
        debug_info: true,
        docs: true
      ]
    ]
  end

  defp docs do
    [
      main: "Zig",
      extras: ["README.md" | guides(Mix.env())],
      groups_for_extras: [Guides: Path.wildcard("guides/*.md")],
      zig_doc: [beam: [file: "priv/beam/beam.zig"]],
    ]
  end

  defp guides(:dev) do
    case File.ls("guides") do
      {:ok, files} ->
        files
        |> Enum.sort()
        |> Enum.filter(&String.ends_with?(&1, ".md"))
        |> Enum.map(&Path.join("guides", &1))

      {:error, _} ->
        []
    end
  end

  defp guides(_), do: []

  def application, do: [extra_applications: [:logger, :inets, :crypto, :public_key, :ssl]]

  defp elixirc_paths(:dev), do: ["lib"]
  defp elixirc_paths(:test), do: ["lib", "test/_support"]
  defp elixirc_paths(_), do: ["lib"]

  def deps do
    [
      # zig parser is pinned to a version of zig parser because versions of zig parser
      # are pinned to zig versions
      {:zig_parser, "~> 0.6"},
      # utility to help manage type protocols
      {:protoss, "~> 1.0"},
      {:zig_get, "== 0.15.2", runtime: false},
      # documentation
      {:markdown_formatter, "~> 0.6", only: :dev, runtime: false},
      {:zig_doc, "~> 0.6", only: :dev, runtime: false}
    ] ++ json()
  end

  defp json do
    case Code.ensure_loaded(JSON) do
      {:module, JSON} ->
        []

      _ ->
        [{:jason, "~> 1.4", runtime: Mix.env() == :test}]
    end
  end
end
