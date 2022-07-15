defmodule AppBuilder do
  def bundle(release) do
    options = validate_options(release.options[:app] || [])

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

  def init do
    {:ok, _} = Registry.register(AppBuilder.Registry, "app_event_subscribers", [])

    if input = System.get_env("APP_BUILDER_INPUT") do
      __rpc__(input)
    end
  end

  def __rpc__ do
    IO.read(:line)
    |> String.trim()
    |> __rpc__()
  end

  def __rpc__("open_app") do
    dispatch(:open_app)
  end

  def __rpc__("open_url:" <> url) do
    dispatch({:open_url, url})
  end

  def __rpc__("open_file:" <> path) do
    path =
      if os() == :windows do
        String.replace(path, "\\", "/")
      else
        path
      end

    dispatch({:open_file, path})
  end

  defp dispatch(message) do
    Registry.dispatch(AppBuilder.Registry, "app_event_subscribers", fn entries ->
      for {pid, _} <- entries, do: send(pid, message)
    end)
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

    options
    |> validate_options(root_allowed_options, os)
    |> Keyword.put_new_lazy(:name, &default_name/0)
    |> Keyword.update!(:document_types, fn document_types ->
      Enum.map(document_types, fn options ->
        validate_options(options, document_type_allowed_options, os)
      end)
    end)
  end

  defp default_name do
    Mix.Project.config()[:app] |> to_string |> Macro.camelize()
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
