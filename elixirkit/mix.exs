defmodule ElixirKit.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixirkit,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      package: package(),
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {ElixirKit.Application, []}
    ]
  end

  def package do
    [
      files: [
        "lib",
        "elixirkit_swift/Package.swift",
        "elixirkit_swift/Sources",
        "elixirkit_dotnet/ElixirKit.csproj",
        "elixirkit_dotnet/ElixirKit.cs",
        "mix.exs",
        "README.md"
      ]
    ]
  end

  defp deps do
    []
  end
end
