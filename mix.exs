defmodule Zigler.MixProject do
  use Mix.Project

  def project do
    env = Mix.env()

    [
      app: :zigler,
      version: "0.9.2",
      elixir: "~> 1.10",
      start_permanent: env == :prod,
      elixirc_paths: elixirc_paths(env),
      deps: deps(),
      aliases: [docs: "zig_doc", test: test_alias()],
      package: [
        description: "Zig nif library",
        licenses: ["MIT"],
        # we need to package the zig BEAM adapters and the c include files as a part
        # of the hex packaging system.
        files: ~w(lib mix.exs README* LICENSE* VERSIONS* priv),
        links: %{
          "GitHub" => "https://github.com/ityonemo/zigler",
          "Zig" => "https://ziglang.org/"
        }
      ],
      dialyzer: [plt_add_deps: :transitive],
      preferred_cli_env: [dialyzer: :dev],
      source_url: "https://github.com/ityonemo/zigler/",
      docs: [
        main: "Zig",
        extras: ["README.md", "guides/nifs.md", "guides/resources.md"],
        groups_for_extras: [Guides: Path.wildcard("guides/*.md")],
        groups_for_modules: ["Under the hood": under_the_hood()]
      ]
    ]
  end

  def under_the_hood do
    [
      Zig.Assembler,
      Zig.Code,
      Zig.Code.LongRunning,
      Zig.Compiler,
      Zig.Module,
      Zig.Parser,
      Zig.Typespec,
      Zig.Command,
      Zig.Doc.Parser,
      Zig.Doc.Retriever,
      Zig.Parser.Error,
      Zig.Parser.Imports,
      Zig.Patches,
      Zig.Parser.Unit,
      Zig.Parser.Resource,
      Zig.Parser.ResourceCleanup,
      Zig.Parser.Nif,
      Zig.Parser
    ]
  end

  def application, do: [extra_applications: [:logger, :inets]]

  defp elixirc_paths(:dev), do: ["lib"]
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp test_alias do
    case :os.type() do
      {:unix, :darwin} -> ["clean", "test"]
      _ -> "test"
    end
  end

  def deps do
    [
      # credo
      {:credo, "~> 1.4.0", only: [:dev, :test], runtime: false},
      # dialyzer
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
      # zigler's parsing is done using nimble_parsec
      {:nimble_parsec, "~> 1.1", runtime: false},
      # to parse the zig JSON
      {:jason, "~> 1.1", runtime: false},
      # documentation
      {:ex_doc, "~> 0.23", runtime: false}
    ]
  end
end
