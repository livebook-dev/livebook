defmodule ElixirKit.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixirkit,
      version: "0.1.0",
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases(),
      docs: docs()
    ]
  end

  def cli do
    [preferred_envs: ["test.all": :test]]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md"]
    ]
  end

  defp aliases do
    [
      docs: &docs/1,
      "docs.rs": &docs_rs/1,
      "docs.all": ["docs", "docs.rs"],
      test: [
        "cmd elixir -e 'Mix.install([{:elixirkit, path: \"#{__DIR__}\"}])'",
        "test"
      ],
      "test.all": [
        "test",
        "test.rs",
        "test.examples"
      ],
      "test.rs": [
        "cmd cargo check --manifest-path elixirkit_rs/Cargo.toml",
        "cmd cargo test --manifest-path elixirkit_rs/Cargo.toml"
      ],
      "test.examples": [
        "cmd ./examples/cli_script.rs"
      ]
    ]
  end

  defp docs(_) do
    readme = File.read!("README.md")
    File.write!("README.md", String.replace(readme, "https://hexdocs.pm/elixirkit/", ""))

    Mix.Task.run("compile")

    try do
      Mix.Task.run("docs")
    after
      File.write!("README.md", readme)
    end
  end

  defp docs_rs(_) do
    case Mix.shell().cmd("cargo doc --no-deps --manifest-path elixirkit_rs/Cargo.toml") do
      0 ->
        File.rm_rf!("doc/rs")
        File.cp_r!("elixirkit_rs/target/doc", "doc/rs")

      status ->
        Mix.raise("cargo doc failed with exit code #{status}")
    end
  end

  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev, warn_if_outdated: true},
      {:makeup_syntect, ">= 0.0.0", only: :dev}
    ]
  end
end
