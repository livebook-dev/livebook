defmodule AppBuilder do
  def bundle(release) do
    options = validate_options(release.options[:app])

    case os() do
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

  defp validate_options(options) do
    os = os()

    root_allowed_options = %{
      all: [
        :name,
        :icon_path,
        url_schemes: [],
        document_types: [],
        additional_paths: []
      ],
      macos: [
        app_type: :regular,
        build_dmg: false,
        notarization: nil
      ],
      windows: [
        :server,
        build_installer: false
      ]
    }

    document_type_allowed_options = %{
      all: [
        :name,
        :extensions,
        :icon_path
      ],
      macos: [
        :role
      ],
      windows: []
    }

    options = validate_options(options, root_allowed_options, os)

    Keyword.update!(options, :document_types, fn document_types ->
      Enum.map(document_types, fn options ->
        validate_options(options, document_type_allowed_options, os)
      end)
    end)
  end

  defp validate_options(options, allowed, os) do
    {macos_options, options} = Keyword.pop(options, :macos, [])
    {windows_options, options} = Keyword.pop(options, :windows, [])

    options_per_os = %{
      macos: macos_options,
      windows: windows_options
    }

    options = Keyword.validate!(options, allowed.all)
    options_for_os = Map.fetch!(options_per_os, os)

    allowed_without_defaults =
      for option <- allowed.all do
        case option do
          atom when is_atom(atom) -> atom
          {atom, _default} when is_atom(atom) -> atom
        end
      end

    allowed_for_os = allowed_without_defaults ++ Map.fetch!(allowed, os)
    os_options = Keyword.validate!(options_for_os, allowed_for_os)
    Keyword.merge(options, os_options)
  end
end
