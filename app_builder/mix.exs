defmodule AppBuilder.MixProject do
  use Mix.Project

  def project do
    [
      app: :app_builder,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Suppress warnings
      xref: [
        exclude: [
          :wx
        ]
      ]
    ]
  end

  def application do
    [
      mod: {AppBuilder.Application, []},
      extra_applications: [:logger, :eex, :inets, :ssl, :crypto]
    ]
  end

  defp deps do
    []
  end
end
