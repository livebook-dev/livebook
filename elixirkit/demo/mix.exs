defmodule Demo.MixProject do
  use Mix.Project

  def project do
    [
      app: :demo,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      releases: releases()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Demo.Application, []}
    ]
  end

  defp deps do
    [
      {:elixirkit, path: ".."}
    ]
  end

  defp releases do
    [
      app: [
        steps: [
          :assemble,
          &ElixirKit.bundle/1
        ],
        app: [
          launcher_path: "rel/Launcher.swift"
        ]
      ]
    ]
  end
end
