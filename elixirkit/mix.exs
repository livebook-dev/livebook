defmodule ElixirKit.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixirkit,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {ElixirKit.Application, []}
    ]
  end

  defp deps do
    []
  end
end
