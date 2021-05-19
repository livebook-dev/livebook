defmodule Livebook.MixProject do
  use Mix.Project

  @version "0.1.0"
  @description "Interactive and collaborative code notebooks - made with Phoenix LiveView"

  def project do
    [
      app: :livebook,
      version: @version,
      elixir: "~> 1.12",
      name: "Livebook",
      description: @description,
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),
      escript: escript(),
      releases: releases(),
      package: package()
    ]
  end

  def application do
    [
      mod: {Livebook.Application, []},
      extra_applications: [:logger, :runtime_tools, :os_mon, :inets, :ssl]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:phoenix, "~> 1.5.7"},
      {:phoenix_live_view, "~> 0.15.0"},
      {:phoenix_live_dashboard, "~> 0.4"},
      {:floki, ">= 0.27.0", only: :test},
      {:phoenix_html, "~> 2.11"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:telemetry_metrics, "~> 0.4"},
      {:telemetry_poller, "~> 0.4"},
      {:jason, "~> 1.0"},
      {:plug_cowboy, "~> 2.0"},
      {:earmark_parser, "~> 1.4"},
      {:bypass, "~> 2.1", only: :test},
      {:castore, "~> 0.1.0"}
    ]
  end

  defp aliases do
    [
      "dev.setup": ["deps.get", "cmd npm install --prefix assets"],
      "dev.build": ["cmd npm run deploy --prefix ./assets"],
      "format.all": ["format", "cmd npm run format --prefix ./assets"],
      # TODO: loadconfig no longer required on Elixir v1.13
      # Currently this ensures we load configuration before
      # compiling dependencies as part of `mix escript.install`.
      # See https://github.com/elixir-lang/elixir/commit/a6eefb244b3a5892895a97b2dad4cce2b3c3c5ed
      "escript.build": ["loadconfig", "escript.build"]
    ]
  end

  defp escript do
    [
      main_module: LivebookCLI,
      app: nil
    ]
  end

  defp releases do
    [
      livebook: [
        include_executables_for: [:unix],
        include_erts: false
      ]
    ]
  end

  def package do
    [
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => "https://github.com/elixir-nx/livebook"
      },
      files: ~w(lib priv/static config mix.exs README.md LICENSE CHANGELOG.md)
    ]
  end
end
