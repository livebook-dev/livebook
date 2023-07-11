defmodule LivebookWeb.AppLive do
  use LivebookWeb, :live_view

  import LivebookWeb.AppHelpers

  @impl true
  def mount(%{"slug" => slug}, _session, socket) when socket.assigns.app_authenticated? do
    if socket.assigns.app_settings.multi_session do
      {:ok, app} = Livebook.Apps.fetch_app(slug)

      if connected?(socket) do
        Livebook.App.subscribe(slug)
      end

      {:ok, assign(socket, app: app)}
    else
      {:ok, pid} = Livebook.Apps.fetch_pid(slug)
      session_id = Livebook.App.get_session_id(pid, user: socket.assigns.current_user)
      {:ok, push_navigate(socket, to: ~p"/apps/#{slug}/#{session_id}")}
    end
  end

  def mount(%{"slug" => slug}, _session, socket) do
    if connected?(socket) do
      {:ok, push_navigate(socket, to: ~p"/apps/#{slug}/authenticate")}
    else
      {:ok, socket}
    end
  end

  @impl true
  def render(assigns) when assigns.app_authenticated? do
    ~H"""
    <div class="h-full relative overflow-y-auto px-4 md:px-20">
      <div class="w-full max-w-screen-lg py-4 mx-auto">
        <div class="absolute md:fixed right-8 md:left-4 top-3 w-10 h-10">
          <.link navigate={~p"/"}>
            <img src={~p"/images/logo.png"} height="40" widthz="40" alt="logo livebook" />
          </.link>
        </div>
        <div class="flex items-center pb-4 mb-2 space-x-4 border-b border-gray-200 pr-20 md:pr-0">
          <h1 class="text-3xl font-semibold text-gray-800">
            <%= @app.notebook_name %>
          </h1>
        </div>
        <div class="pt-4 flex flex-col space-y-16">
          <.content_skeleton :for={_idx <- 1..5} empty={false} />
        </div>
      </div>
    </div>

    <.modal id="sessions-modal" show width={:big} patch={~p"/"}>
      <div class="p-6 max-w-4xl flex flex-col space-y-3">
        <h3 class="text-2xl font-semibold text-gray-800">
          <%= @app.notebook_name %>
        </h3>
        <p class="text-gray-700">
          <%= if @app_settings.show_existing_sessions do %>
            This is a multi-session app, pick an existing session or create a new one.
          <% else %>
            This is a multi-session app, create a new one to get started.
          <% end %>
        </p>
        <div class="flex justify-end">
          <.link class="button-base button-outlined-blue" patch={~p"/apps/#{@app.slug}/new"}>
            <.remix_icon icon="add-line" class="align-middle mr-1" />
            <span>New session</span>
          </.link>
        </div>
        <div :if={@app_settings.show_existing_sessions} class="w-full flex flex-col space-y-4">
          <.link
            :for={app_session <- active_sessions(@app.sessions)}
            navigate={~p"/apps/#{@app.slug}/#{app_session.id}"}
            class="px-4 py-3 border border-gray-200 rounded-xl text-gray-800 pointer hover:bg-gray-50 flex justify-between"
          >
            <span>
              Started
              <span :if={app_session.started_by}>
                by
                <span class="font-semibold"><%= app_session.started_by.name || "Anonymous" %></span>
              </span>
              <%= format_datetime_relatively(app_session.created_at) %> ago
            </span>
            <div class="mr-0.5 flex">
              <.app_status status={app_session.app_status} show_label={false} />
            </div>
          </.link>
        </div>
      </div>
    </.modal>
    """
  end

  def render(assigns), do: auth_placeholder(assigns)

  @impl true
  def handle_params(_params, _url, socket) when socket.assigns.live_action == :new_session do
    session_id =
      Livebook.App.get_session_id(socket.assigns.app.pid, user: socket.assigns.current_user)

    {:noreply, push_navigate(socket, to: ~p"/apps/#{socket.assigns.app.slug}/#{session_id}")}
  end

  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_info({:app_updated, app}, socket) do
    {:noreply, assign(socket, :app, app)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp active_sessions(sessions) do
    Enum.filter(sessions, &(&1.app_status.lifecycle == :active))
  end
end
