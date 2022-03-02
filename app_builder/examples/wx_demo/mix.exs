defmodule WxDemo.MixProject do
  use Mix.Project

  def project do
    [
      app: :wx_demo,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      releases: releases()
    ]
  end

  def application do
    [
      extra_applications: [:wx, :logger],
      mod: {WxDemo.Application, []}
    ]
  end

  defp deps do
    [
      {:app_builder, path: "../.."}
    ]
  end

  defp releases do
    options = [
      name: "WxDemo",
      url_schemes: ["wxdemo"],
      document_types: [
        %{
          name: "WxDemo",
          extensions: ["wxdemo"],
          # macos specific
          role: "Editor"
        }
      ]
    ]

    [
      mac_app: [
        include_executables_for: [:unix],
        steps: [:assemble, &AppBuilder.build_mac_app(&1, options)]
      ],
      mac_app_dmg: [
        include_executables_for: [:unix],
        steps: [:assemble, &build_mac_app_dmg(&1, options)]
      ],
      windows_installer: [
        include_executables_for: [:windows],
        steps: [
          :assemble,
          &AppBuilder.build_windows_installer(&1, [module: WxDemo.Window] ++ options)
        ]
      ]
    ]
  end

  defp build_mac_app_dmg(release, options) do
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
      ] ++ options

    AppBuilder.build_mac_app_dmg(release, options)
  end
end
