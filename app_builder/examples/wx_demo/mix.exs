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
    macos_notarization = macos_notarization()

    [
      app: [
        steps: [
          :assemble,
          &AppBuilder.bundle/1
        ],
        app: [
          name: "WxDemo",
          url_schemes: ["wxdemo"],
          document_types: [
            [
              name: "WxDemo",
              extensions: ["wxdemo"],
              macos: [
                role: "Editor"
              ]
            ]
          ],
          event_handler: WxDemo.Window,
          macos: [
            build_dmg: macos_notarization != nil,
            notarization: macos_notarization
          ],
          windows: [
            build_installer: true
          ]
        ]
      ]
    ]
  end

  defp macos_notarization do
    identity = System.get_env("NOTARIZE_IDENTITY")
    team_id = System.get_env("NOTARIZE_TEAM_ID")
    apple_id = System.get_env("NOTARIZE_APPLE_ID")
    password = System.get_env("NOTARIZE_PASSWORD")

    if identity && team_id && apple_id && password do
      [identity: identity, team_id: team_id, apple_id: apple_id, password: password]
    end
  end
end
