if Mix.target() == :app do
  defmodule LivebookApp do
    @moduledoc false

    @behaviour :wx_object

    # https://github.com/erlang/otp/blob/OTP-24.1.2/lib/wx/include/wx.hrl#L1314
    @wx_id_exit 5006

    def start_link(_) do
      {:wx_ref, _, _, pid} = :wx_object.start_link(__MODULE__, [], [])
      {:ok, pid}
    end

    def child_spec(init_arg) do
      %{
        id: __MODULE__,
        start: {__MODULE__, :start_link, [init_arg]},
        restart: :transient
      }
    end

    @impl true
    def init(_) do
      title = "Livebook"

      wx = :wx.new()
      frame = :wxFrame.new(wx, -1, title, size: {0, 0})

      if macos?() do
        fixup_macos_menubar(frame, title)
      end

      :wxFrame.show(frame)
      :wxFrame.connect(frame, :command_menu_selected)
      :wxFrame.connect(frame, :close_window, skip: true)
      :wx.subscribe_events()
      state = %{frame: frame}

      Livebook.Utils.browser_open(LivebookWeb.Endpoint.access_url())

      {frame, state}
    end

    @impl true
    def handle_event({:wx, @wx_id_exit, _, _, _}, state) do
      :init.stop()
      {:stop, :normal, state}
    end

    @impl true
    def handle_event({:wx, _, _, _, {:wxClose, :close_window}}, state) do
      :init.stop()
      {:stop, :normal, state}
    end

    # TODO: investigate "Universal Links" [1], that is, instead of livebook://foo, we have
    # https://livebook.dev/foo, which means the link works with and without Livebook.app.
    #
    # [1] https://developer.apple.com/documentation/xcode/allowing-apps-and-websites-to-link-to-your-content
    @impl true
    def handle_info({:open_url, url}, state) do
      'livebook://' ++ rest = url
      import_livebook("https://#{rest}")
      {:noreply, state}
    end

    @impl true
    def handle_info({:open_file, path}, state) do
      import_livebook("file://#{path}")
      {:noreply, state}
    end

    # ignore other events
    @impl true
    def handle_info(_event, state) do
      {:noreply, state}
    end

    defp import_livebook(url) do
      LivebookWeb.Endpoint.access_struct_url()
      |> Map.replace!(:path, "/import")
      |> append_query("url=#{URI.encode_www_form(url)}")
      |> URI.to_string()
      |> Livebook.Utils.browser_open()
    end

    defp fixup_macos_menubar(frame, title) do
      menubar = :wxMenuBar.new()
      :wxFrame.setMenuBar(frame, menubar)

      # App Menu
      menu = :wxMenuBar.oSXGetAppleMenu(menubar)

      # Remove all items except for quit
      for item <- :wxMenu.getMenuItems(menu) do
        if :wxMenuItem.getId(item) == @wx_id_exit do
          :wxMenuItem.setText(item, "Quit #{title}\tCtrl+Q")
        else
          :wxMenu.delete(menu, item)
        end
      end
    end

    defp macos?() do
      :os.type() == {:unix, :darwin}
    end

    # TODO: On Elixir v1.14, use URI.append_query/2
    defp append_query(%URI{query: query} = uri, query_to_add) when query in [nil, ""] do
      %{uri | query: query_to_add}
    end

    defp append_query(%URI{} = uri, query) do
      %{uri | query: uri.query <> "&" <> query}
    end
  end
end
