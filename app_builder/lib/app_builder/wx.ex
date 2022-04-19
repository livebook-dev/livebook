defmodule AppBuilder.Wx do
  require Logger

  @doc """
  Subscribe the given `server` to application events.

  The possible events are:

    * `{:new_file, []}`
    * `{:reopen_app, []}`
    * `{:open_file, path}`
    * `{:open_url, url}`

  On macOS, we simply call `:wx.subscribe_events()`. On Windows,
  we emulate it.
  """
  def subscribe_to_app_events(server) do
    unless Code.ensure_loaded?(:wx) do
      Logger.error("""
      wx is not available.

      Please add it to your extra applications:

          extra_applications: [:wx]
      """)

      raise "wx is not available"
    end

    case :os.type() do
      {:unix, :darwin} ->
        :wx.subscribe_events()

      {:win32, _} ->
        input = System.get_env("APP_BUILDER_INPUT", "new_file")
        AppBuilder.Windows.__send_events__(server, input)
    end
  end
end
