defmodule Livebook.MixProject do
  use Mix.Project

  @elixir_requirement "~> 1.13"
  @app_elixir_version "1.13.2"
  @version "0.5.2"
  @description "Interactive and collaborative code notebooks - made with Phoenix LiveView"

  def project do
    [
      app: :livebook,
      version: @version,
      elixir: @elixir_requirement,
      name: "Livebook",
      description: @description,
      elixirc_paths: elixirc_paths(Mix.env()),
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
      extra_applications:
        [:logger, :runtime_tools, :os_mon, :inets, :ssl, :xmerl] ++
          extra_applications(Mix.target()),
      env: Application.get_all_env(:livebook)
    ]
  end

  defp extra_applications(:app), do: [:wx]
  defp extra_applications(_), do: []

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp package do
    [
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => "https://github.com/livebook-dev/livebook"
      },
      files: ~w(lib static config mix.exs mix.lock README.md LICENSE CHANGELOG.md)
    ]
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
      {:phoenix, "~> 1.5"},
      {:phoenix_html, "~> 3.0"},
      {:phoenix_live_view, "~> 0.17.3"},
      {:phoenix_live_dashboard, "~> 0.6.0"},
      {:telemetry_metrics, "~> 0.4"},
      {:telemetry_poller, "~> 1.0"},
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

  defp target_deps(:app), do: [{:app_builder, path: "app_builder"}]
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
        include_executables_for: [:unix],
        include_erts: false,
        rel_templates_path: "rel/server",
        steps: [:assemble, &remove_cookie/1]
      ],
      mac_app: [
        include_executables_for: [:unix],
        include_erts: false,
        rel_templates_path: "rel/app",
        steps: [:assemble, &remove_cookie/1, &standalone_erlang_elixir/1, &build_mac_app/1]
      ],
      mac_app_dmg: [
        include_executables_for: [:unix],
        include_erts: false,
        rel_templates_path: "rel/app",
        steps: [:assemble, &remove_cookie/1, &standalone_erlang_elixir/1, &build_mac_app_dmg/1]
      ]
    ]
  end

  defp remove_cookie(release) do
    File.rm!(Path.join(release.path, "releases/COOKIE"))
    release
  end

  defp standalone_erlang_elixir(release) do
    Code.require_file("rel/app/standalone.exs")

    release
    |> Standalone.copy_erlang()
    |> Standalone.copy_elixir(@app_elixir_version)
  end

  @app_options [
    name: "Livebook",
    version: @version,
    logo_path: "rel/app/mac-icon.png",
    url_schemes: ["livebook"],
    additional_paths: ["/rel/vendor/bin", "/rel/vendor/elixir/bin"],
    document_types: [
      %{name: "LiveMarkdown", role: "Editor", extensions: ["livemd"]}
    ]
  ]

  defp build_mac_app(release) do
    AppBuilder.build_mac_app(release, @app_options)
  end

  defp build_mac_app_dmg(release) do
    options =
      [
        codesign: [
          identity: System.fetch_env!("CODESIGN_IDENTITY")
        ],
        notarize: [
          team_id: System.fetch_env!("NOTARIZE_TEAM_ID"),
          apple_id: System.fetch_env!("NOTARIZE_APPLE_ID"),
          password: System.fetch_env!("NOTARIZE_PASSWORD")
        ]
      ] ++ @app_options

    AppBuilder.build_mac_app_dmg(release, options)
  end
end
