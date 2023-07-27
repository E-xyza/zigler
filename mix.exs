defmodule Zigler.MixProject do
  use Mix.Project

  def project do
    env = Mix.env()

    [
      app: :zigler,
      version: "0.10.1",
      elixir: "~> 1.13",
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
          "GitHub" => "https://github.com/ityonemo/zigler",
          "Zig" => "https://ziglang.org/"
        }
      ],
      dialyzer: [plt_add_deps: :transitive],
      preferred_cli_env: [dialyzer: :dev],
      source_url: "https://github.com/ityonemo/zigler/",
      docs: docs(),
      aliases: [docs: "zig_doc"]
    ]
  end

  defp docs do
    [
      main: "Zig",
      # , "guides/resources.md"],
      extras: ["README.md" | guides()],
      groups_for_extras: [Guides: Path.wildcard("guides/*.md")],
      groups_for_modules: [
        "Compilation Steps": compilation_steps(),
        Types: ~r/Zig\.Type(^\.ParseError)/,
        "Nif Modes": [~r/Zig\.Nif.+/, Zig.EasyC],
        "Data Structures": data_structures(),
        tools: tools()
      ],
      zig_doc: [beam: [file: "priv/beam/beam.zig"]]
    ]
  end

  defp guides do
    "guides"
    |> File.ls!()
    |> Enum.sort()
    |> Enum.filter(&String.ends_with?(&1, ".md"))
    |> Enum.map(&Path.join("guides", &1))
  end

  defp compilation_steps do
    [
      Zig.Assembler,
      Zig.Builder,
      Zig.Command,
      Zig.Compiler,
      Zig.Sema
    ]
  end

  defp data_structures do
    [
      Zig.Module,
      Zig.Nif,
      Zig.Manifest,
      Zig.Resources
    ]
  end

  defp tools do
    [
      Zig.Analyzer,
      Zig.Macro,
      Zig.Options,
      Zig.QuoteErl,
      Zig.ErrorProng,
      Zig.Target
    ]
  end

  def application, do: [extra_applications: [:logger, :inets, :crypto, :public_key, :ssl]]

  defp elixirc_paths(:dev), do: ["lib"]
  defp elixirc_paths(:test), do: ["lib", "test/_support"]
  defp elixirc_paths(_), do: ["lib"]

  def deps do
    [
      # credo
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      # dialyzer
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
      # to parse the zig JSON
      {:jason, "~> 1.4"},
      # zig parser is pinned to a version of zig parser because versions of zig parser
      # are pinned to zig versions
      {:zig_parser, "== 0.1.8"},
      # documentation
      {:ex_doc, "~> 0.30.0", only: :dev, runtime: false},
      {:zig_doc, "~> 0.1.3", only: :dev, runtime: false}
    ]
  end
end
