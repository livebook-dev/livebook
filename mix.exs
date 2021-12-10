defmodule Livebook.MixProject do
  use Mix.Project

  @version "0.4.1"
  @description "Interactive and collaborative code notebooks - made with Phoenix LiveView"

  def project do
    [
      app: :livebook,
      version: @version,
      elixir: "~> 1.13",
      name: "Livebook",
      description: @description,
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: with_lock(deps()),
      escript: escript(),
      releases: releases(),
      package: package()
    ]
  end

  def application do
    [
      mod: {Livebook.Application, []},
      extra_applications: [:logger, :runtime_tools, :os_mon, :inets, :ssl, :xmerl],
      env: Application.get_all_env(:livebook)
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Although we use requirements here, the with_lock() function
  # below ensures we only use the locked versions. This is important
  # for two reasons:
  #
  #   1. because we bundle assets from phoenix, phoenix_live_view,
  #      and phoenix_html, we want to make sure we have those exact
  #      versions
  #
  #   2. we don't want users to potentially get a new dependency
  #      when installing from git or as an escript
  #
  # Therefore, to update any dependency, you must call before:
  #
  #     mix deps.unlock foo bar baz
  #
  defp deps do
    [
      {:phoenix, "~> 1.5"},
      {:phoenix_html, "~> 3.0"},
      {:phoenix_live_view, "~> 0.17.3"},
      {:phoenix_live_dashboard, "~> 0.6.0"},
      {:telemetry_metrics, "~> 0.4"},
      {:telemetry_poller, "~> 0.4"},
      {:jason, "~> 1.0"},
      {:plug_cowboy, "~> 2.0"},
      {:earmark_parser, "~> 1.4"},
      {:castore, "~> 0.1.0"},
      {:aws_signature, "~> 0.2.0"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:floki, ">= 0.27.0", only: :test},
      {:bypass, "~> 2.1", only: :test}
    ]
  end

  @lock (with {:ok, contents} <- File.read("mix.lock"),
              {:ok, quoted} <- Code.string_to_quoted(contents, warn_on_unnecessary_quotes: false),
              {%{} = lock, _binding} <- Code.eval_quoted(quoted, []) do
           for {dep, hex} when elem(hex, 0) == :hex <- lock,
               do: {dep, elem(hex, 2)},
               into: %{}
         else
           _ -> %{}
         end)

  defp with_lock(deps) do
    for dep <- deps do
      name = elem(dep, 0)
      put_elem(dep, 1, @lock[name] || elem(dep, 1))
    end
  end

  defp aliases do
    [
      "dev.setup": ["deps.get", "cmd npm install --prefix assets"],
      "dev.build": ["cmd npm run deploy --prefix ./assets"],
      "format.all": ["format", "cmd npm run format --prefix ./assets"]
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
        "GitHub" => "https://github.com/livebook-dev/livebook"
      },
      files: ~w(lib static config mix.exs mix.lock README.md LICENSE CHANGELOG.md)
    ]
  end
end
