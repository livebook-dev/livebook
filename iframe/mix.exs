defmodule LivebookSpace.MixProject do
  use Mix.Project

  def project do
    [
      app: :livebook_space,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {LivebookSpace.Application, []}
    ]
  end

  defp deps do
    [
      {:plug_cowboy, "~> 2.0"}
    ]
  end
end
