defmodule AppBuilder do
  def bundle(release) do
    os = os()

    allowed_options = [
      :name,
      :server,
      icon_path: [
        macos: Application.app_dir(:wx, "examples/demo/erlang.png")
      ],
      url_schemes: [],
      document_types: [],
      additional_paths: [],
      macos_is_agent_app: false,
      macos_build_dmg: false,
      macos_notarization: nil,
      windows_build_installer: true
    ]

    options = Keyword.validate!(release.options[:app], allowed_options)

    case os do
      :macos ->
        AppBuilder.MacOS.bundle(release, options)

      :windows ->
        AppBuilder.Windows.bundle(release, options)
    end
  end

  def os do
    case :os.type() do
      {:unix, :darwin} -> :macos
      {:win32, _} -> :windows
    end
  end
end
