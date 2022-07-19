defmodule Demo.MixProject do
  use Mix.Project

  def project do
    [
      app: :demo,
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
      mod: {Demo.Application, []}
    ]
  end

  defp deps do
    [
      {:app_bundler, path: ".."}
    ]
  end

  defp releases do
    macos_notarization = macos_notarization()

    [
      app: [
        steps: [
          :assemble,
          &AppBundler.bundle/1
        ],
        app: [
          name: "Demo",
          url_schemes: ["wxdemo"],
          document_types: [
            [
              name: "Demo",
              extensions: ["wxdemo"],
              macos: [
                role: "Editor"
              ]
            ]
          ],
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
