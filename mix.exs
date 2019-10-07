defmodule Zigler.MixProject do
  use Mix.Project

  def project do
    [
      app: :zigler,
      version: "0.1.0",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  def deps do
    # we need this for mix zigler.get_zig mix task.
    [{:mojito, "~> 0.5.0", only: :dev, runtime: :false}]
  end
end
