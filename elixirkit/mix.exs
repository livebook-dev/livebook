defmodule ElixirKit.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixirkit,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      compilers: [:elixir_make] ++ Mix.compilers(),
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :eex],
      mod: {ElixirKit.Application, []}
    ]
  end

  defp deps do
    [
      {:elixir_make, "~> 0.6"}
    ]
  end
end
