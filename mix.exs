defmodule Livebook.MixProject do
  use Mix.Project

  @elixir_requirement "~> 1.14.2 or ~> 1.15-dev"
  @version "0.11.0-dev"
  @description "Automate code & data workflows with interactive notebooks"

  def project do
    [
      app: :livebook,
      version: @version,
      elixir: @elixir_requirement,
      name: "Livebook",
      description: @description,
      elixirc_paths: elixirc_paths(Mix.env()),
      test_elixirc_options: [docs: true],
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: with_lock(target_deps(Mix.target()) ++ deps()),
      escript: escript(),
      package: package(),
      default_release: :livebook,
      releases: releases()
    ]
  end

  def application do
    [
      mod: {Livebook.Application, []},
      extra_applications: [
        :logger,
        :runtime_tools,
        :os_mon,
        :inets,
        :ssl,
        :xmerl,
        :crypto,
        :public_key
      ],
      env: Application.get_all_env(:livebook)
    ]
  end

  defp elixirc_paths(:test), do: elixirc_paths(:dev) ++ ["test/support"]
  defp elixirc_paths(_), do: ["lib", "proto/lib"]

  defp package do
    [
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => "https://github.com/livebook-dev/livebook"
      },
      files:
        ~w(lib static config mix.exs mix.lock README.md LICENSE CHANGELOG.md iframe/priv/static/iframe proto/lib)
    ]
  end

  defp aliases do
    [
      setup: ["deps.get", "cmd npm install --prefix assets"],
      "assets.deploy": ["cmd npm run deploy --prefix assets"],
      "format.all": ["format", "cmd npm run format --prefix assets"],
      "protobuf.generate": ["cmd --cd proto mix protobuf.generate"]
    ]
  end

  defp escript do
    [
      main_module: LivebookCLI,
      app: nil
    ]
  end

  ## Dependencies

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
      {:phoenix, "~> 1.7.0"},
      {:phoenix_html, "~> 3.0"},
      # {:phoenix_live_view, "~> 0.19.0"},
      {:phoenix_live_view, github: "phoenixframework/phoenix_live_view", override: true},
      {:phoenix_live_dashboard, "~> 0.8.0"},
      {:telemetry_metrics, "~> 0.4"},
      {:telemetry_poller, "~> 1.0"},
      {:jason, "~> 1.0"},
      {:plug_cowboy, "~> 2.0"},
      {:earmark_parser, "~> 1.4"},
      {:castore, "~> 1.0"},
      {:ecto, "~> 3.10"},
      {:phoenix_ecto, "~> 4.4"},
      {:aws_signature, "~> 0.3.0"},
      {:mint_web_socket, "~> 1.0.0"},
      {:protobuf, "~> 0.8.0"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:floki, ">= 0.27.0", only: :test},
      {:bypass, "~> 2.1", only: :test},
      # ZTA deps
      {:jose, "~> 1.11.5"},
      {:req, "~> 0.3.8"}
    ]
  end

  defp target_deps(:app), do: [{:elixirkit, path: "elixirkit"}]
  defp target_deps(_), do: []

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

  ## Releases

  defp releases do
    [
      livebook: [
        include_executables_for: [:unix, :windows],
        include_erts: false,
        rel_templates_path: "rel/server",
        steps: [:assemble, &remove_cookie/1]
      ],
      app: [
        include_erts: false,
        rel_templates_path: "rel/app",
        steps: [
          :assemble,
          &remove_cookie/1,
          &standalone_erlang_elixir/1
        ]
      ]
    ]
  end

  defp remove_cookie(release) do
    File.rm!(Path.join(release.path, "releases/COOKIE"))
    release
  end

  @compile {:no_warn_undefined, Standalone}

  defp standalone_erlang_elixir(release) do
    {_, bindings} = Code.eval_file("versions")
    elixir_version = bindings[:elixir]
    rebar3_version = bindings[:rebar3]

    Code.require_file("rel/app/standalone.exs")

    release
    |> Standalone.copy_otp()
    |> Standalone.copy_elixir(elixir_version)
    |> Standalone.copy_hex()
    |> Standalone.copy_rebar3(rebar3_version)
  end
end
