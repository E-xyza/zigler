defmodule Zigler.MixProject do
  use Mix.Project

  def project do
    [
      app: :zigler,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: [doc: "zig_doc"]
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
      # zigler's parsing is done using nimble_parsec
      {:nimble_parsec, "~> 0.5", runtime: false},
      # we need this for mix zigler.get_zig mix task.
      {:mojito, "~> 0.5.0", only: :dev, runtime: false},
      # documentation
      {:ex_doc, "~> 0.21", only: [:dev, :doc], runtime: false},
    ]
  end
end
