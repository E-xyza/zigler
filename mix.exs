defmodule Zigler.MixProject do
  use Mix.Project

  def zig_version, do: "0.17.0"

  def project do
    env = Mix.env()

    [
      app: :zigler,
      version: "0.17.0",
      elixir: "~> 1.18",
      start_permanent: env == :prod,
      elixirc_paths: elixirc_paths(env),
      deps: deps(),
      package: [
        description: "Zig nif library",
        licenses: ["MIT"],
        # we need to package the zig BEAM adapters and the c include files as a part
        # of the hex packaging system.
        files: ~w[lib mix.exs README* LICENSE* VERSIONS* priv/beam priv/erl_nif_win],
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
      ],
      # Ignore helper modules that start with underscore (they define modules used by tests)
      test_ignore_filters: [&String.contains?(&1, "/_")]
    ]
  end

  defp docs do
    [
      main: "Zig",
      extras: ["README.md" | guides(Mix.env())],
      groups_for_extras: [Guides: Path.wildcard("guides/*.md")],
      zig_doc: [beam: [file: "priv/beam/beam.zig"]]
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
      {:zig_parser, "~> 0.8.0"},
      # utility to help manage type protocols
      {:protoss, "~> 1.0"},
      {:zig_get, "~> 0.17.0", runtime: false},
      # Zig 0.17 deprecates the built-in std.Build.Step.TranslateC in favour of the
      # ZSF translate-c package, which in turn needs the aro C frontend.  Neither is
      # an elixir project, so they are fetched as source-only git deps and handed to
      # `zig build` as path dependencies -- this keeps nif compilation offline, with
      # the exact commits pinned in mix.lock.
      {:translate_c, git: "https://codeberg.org/ziglang/translate-c.git", branch: "zig-0.17.x",
       compile: false, app: false, runtime: false},
      {:arocc, git: "https://codeberg.org/ziglang/arocc.git",
       ref: "d0c8c4d9c55daa7ef6e40cf0f630a5b5e900989b",
       compile: false, app: false, runtime: false},
      # documentation
      {:markdown_formatter, "~> 0.6", only: :dev, runtime: false},
      {:zig_doc, "~> 0.8.0"},
      # linting
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false}
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
