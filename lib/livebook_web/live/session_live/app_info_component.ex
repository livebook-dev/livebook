defmodule LivebookWeb.SessionLive.AppInfoComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.AppComponents

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col">
      <div class="flex items-center justify-between">
        <h3 class="uppercase text-sm font-semibold text-gray-500">
          App
        </h3>
        <.app_info_icon />
      </div>
      <%= if @session.mode == :app do %>
        <div class="mt-5 flex flex-col">
          <.message_box
            kind={:info}
            message="This session is a running app. To deploy a modified version, you can fork it."
          />
          <div class="mt-6">
            <.button phx-click="fork_session">
              <.remix_icon icon="git-branch-line" />
              <span>Fork</span>
            </.button>
          </div>
        </div>
      <% else %>
        <div class="flex flex-col gap-3 mt-2">
          <.message_box
            :if={@any_session_secrets?}
            kind={:warning}
            message="The notebook uses session secrets, but those are not available to deployed apps. Convert them to Hub secrets instead."
          />

          <div class="flex flex-col space-y-3">
            <.labeled_text label="Slug" one_line>
              <%= @settings.slug || "?" %>
            </.labeled_text>

            <.labeled_text label="Session type" one_line>
              <%= if @settings.multi_session, do: "Multi", else: "Single" %>
            </.labeled_text>

            <span class="text-sm text-gray-500">
              <%= if @settings.access_type == :public do %>
                <.remix_icon icon="lock-unlock-line" /> No password
              <% else %>
                <.remix_icon icon="lock-password-line" /> Password protected
              <% end %>
            </span>
          </div>

          <.button color="gray" outlined patch={~p"/sessions/#{@session.id}/settings/app"}>
            Configure
          </.button>
        </div>

        <h3 class="mt-12 uppercase text-sm font-semibold text-gray-500">
          Remote deployment
        </h3>

        <div class="mt-2 flex flex-col gap-2">
          <.button color="blue" patch={~p"/sessions/#{@session.id}/app-docker"}>
            <.remix_icon icon="rocket-line" /> Deploy with Livebook Teams
          </.button>

          <.button color="gray" outlined patch={~p"/sessions/#{@session.id}/app-docker"}>
            <.remix_icon icon="ship-line" /> Manual Docker deployment
          </.button>
        </div>

        <h3 class="mt-12 uppercase text-sm font-semibold text-gray-500">
          Local preview
        </h3>

        <div class="flex flex-col mt-2 space-y-4">
          <div :if={@app} class="flex flex-col space-y-3">
            <.labeled_text label="URL" one_line>
              <a href={~p"/apps/#{@app.slug}"}>
                <%= ~p"/apps/#{@app.slug}" %>
              </a>
            </.labeled_text>

            <.labeled_text :if={@app.multi_session} label="Latest version" one_line>
              v<%= @app.version %>
            </.labeled_text>

            <div :if={@app.sessions != []}>
              <span class="text-sm text-gray-500">Running sessions</span>

              <div class="mt-2 flex flex-col space-y-4">
                <.app_sessions app={@app} myself={@myself} />
              </div>
            </div>
          </div>

          <div class="flex space-x-2">
            <span
              class={[
                "grow",
                not Livebook.Notebook.AppSettings.valid?(@settings) && "tooltip top-right"
              ]}
              data-tooltip="You must configure the app to preview it"
            >
              <.button
                color={if(@app, do: "blue", else: "gray")}
                outlined={!@app}
                phx-click="deploy_app"
                disabled={not Livebook.Notebook.AppSettings.valid?(@settings)}
              >
                <.remix_icon icon="slideshow-4-line" />
                <span><%= if @app, do: "Relaunch", else: "Launch preview" %></span>
              </.button>
            </span>

            <.button
              :if={@app}
              color="red"
              outlined
              type="button"
              phx-click="terminate_app"
              phx-target={@myself}
            >
              Terminate
            </.button>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  defp app_sessions(assigns) do
    ~H"""
    <div :for={app_session <- @app.sessions} class="w-full border border-gray-200 rounded-lg">
      <div class="px-4 py-3 flex gap-3">
        <.labeled_text label="Status" class="grow">
          <a
            class="inline-block"
            aria-label="debug app"
            href={app_session.app_status == :error && ~p"/sessions/#{app_session.id}"}
            target="_blank"
          >
            <.app_status status={app_session.app_status} />
          </a>
        </.labeled_text>
        <.labeled_text label="Version" class="grow">
          v<%= app_session.version %>
        </.labeled_text>
      </div>
      <div class="border-t border-gray-200 px-3 py-1 flex space-x-2">
        <div class="grow" />
        <span class="tooltip top" data-tooltip="Open">
          <.icon_button
            disabled={app_session.app_status.lifecycle != :active}
            aria-label="open app"
            href={~p"/apps/#{@app.slug}/#{app_session.id}"}
          >
            <.remix_icon icon="link" />
          </.icon_button>
        </span>
        <span class="tooltip top" data-tooltip="Debug">
          <.icon_button aria-label="debug app" href={~p"/sessions/#{app_session.id}"}>
            <.remix_icon icon="terminal-line" />
          </.icon_button>
        </span>
        <%= if app_session.app_status.lifecycle == :active do %>
          <span class="tooltip top" data-tooltip="Deactivate">
            <.icon_button
              aria-label="deactivate app session"
              phx-click={
                JS.push("deactivate_app_session",
                  value: %{session_id: app_session.id},
                  target: @myself
                )
              }
            >
              <.remix_icon icon="stop-circle-line" />
            </.icon_button>
          </span>
        <% else %>
          <span class="tooltip top" data-tooltip="Terminate">
            <.icon_button
              aria-label="terminate app session"
              phx-click={
                JS.push("terminate_app_session",
                  value: %{session_id: app_session.id},
                  target: @myself
                )
              }
            >
              <.remix_icon icon="delete-bin-6-line" />
            </.icon_button>
          </span>
        <% end %>
      </div>
    </div>
    """
  end

  defp app_info_icon(assigns) do
    ~H"""
    <span
      class="tooltip bottom-left"
      data-tooltip={
        ~S'''
        App deployment is a way to share your
        notebook for people to interact with. Use
        inputs and controls to build interactive
        UIs, perfect for demos and tasks.
        '''
      }
    >
      <.icon_button>
        <.remix_icon icon="question-line" />
      </.icon_button>
    </span>
    """
  end

  @impl true
  def handle_event("terminate_app", %{}, socket) do
    {:noreply, confirm_app_termination(socket, socket.assigns.app.pid, "preview")}
  end

  def handle_event("terminate_app_session", %{"session_id" => session_id}, socket) do
    app_session = Enum.find(socket.assigns.app.sessions, &(&1.id == session_id))
    Livebook.Session.close(app_session.pid)
    {:noreply, socket}
  end

  def handle_event("deactivate_app_session", %{"session_id" => session_id}, socket) do
    app_session = Enum.find(socket.assigns.app.sessions, &(&1.id == session_id))
    Livebook.Session.app_deactivate(app_session.pid)
    {:noreply, socket}
  end
end
