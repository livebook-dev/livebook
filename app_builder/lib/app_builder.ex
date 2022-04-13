defmodule AppBuilder do
  def setup_wx(server) do
    case :os.type() do
      {:unix, :darwin} ->
        :wx.subscribe_events()

      {:win32, _} ->
        input = System.get_env("APP_BUILDER_INPUT", "")
        AppBuilder.Windows.send_wx_events(server, input)
    end
  end

  defdelegate build_mac_app(release, options), to: AppBuilder.MacOS

  defdelegate build_mac_app_dmg(release, options), to: AppBuilder.MacOS

  defdelegate build_windows_installer(release, options), to: AppBuilder.Windows
end
