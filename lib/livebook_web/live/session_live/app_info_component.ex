defmodule LivebookWeb.SessionLive.AppInfoComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.AppHelpers

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
            <button class="button-base button-blue" phx-click="fork_session">
              <.remix_icon icon="git-branch-line" />
              <span>Fork</span>
            </button>
          </div>
        </div>
      <% else %>
        <div class="mt-5 flex flex-col gap-6">
          <.message_box
            :if={@any_session_secrets?}
            kind={:warning}
            message="You defined session secrets, but those are not available to the deployed app, only Hub secrets are."
          />
          <div class="flex space-x-2">
            <button
              class="button-base button-blue"
              phx-click="deploy_app"
              disabled={not Livebook.Notebook.AppSettings.valid?(@settings)}
            >
              <.remix_icon icon="rocket-line" class="align-middle mr-1" />
              <span>Deploy</span>
            </button>
            <.link
              patch={~p"/sessions/#{@session.id}/settings/app"}
              class="button-base button-outlined-gray bg-transparent"
            >
              Configure
            </.link>
          </div>
        </div>
        <%= if @app do %>
          <h3 class="mt-10 uppercase text-sm font-semibold text-gray-500">
            Latest deployment
          </h3>
          <div class="mt-2 border border-gray-200 rounded-lg">
            <div class="p-4 flex flex-col space-y-3">
              <.labeled_text label="URL" one_line>
                <a href={~p"/apps/#{@app.slug}"}>
                  <%= ~p"/apps/#{@app.slug}" %>
                </a>
              </.labeled_text>
              <.labeled_text label="Version" one_line>
                v<%= @app.version %>
              </.labeled_text>
              <.labeled_text label="Session type" one_line>
                <%= if(@app.multi_session, do: "Multi", else: "Single") %>
              </.labeled_text>
            </div>
            <div class="border-t border-gray-200 px-3 py-2 flex space-x-2">
              <div class="grow" />
              <span class="tooltip top" data-tooltip="Terminate">
                <button
                  class="icon-button"
                  aria-label="terminate app"
                  phx-click={JS.push("terminate_app", target: @myself)}
                >
                  <.remix_icon icon="delete-bin-6-line" class="text-lg" />
                </button>
              </span>
            </div>
          </div>
          <h3 class="mt-10 uppercase text-sm font-semibold text-gray-500">
            Running sessions
          </h3>
          <div class="mt-2 flex flex-col space-y-4">
            <div :for={app_session <- @app.sessions} class="border border-gray-200 rounded-lg">
              <div class="p-4 flex flex-col space-y-3">
                <.labeled_text label="Status">
                  <a
                    class="inline-block"
                    aria-label="debug app"
                    href={app_session.app_status == :error && ~p"/sessions/#{app_session.id}"}
                    target="_blank"
                  >
                    <.app_status status={app_session.app_status} />
                  </a>
                </.labeled_text>
                <.labeled_text label="Version">
                  v<%= app_session.version %>
                </.labeled_text>
              </div>
              <div class="border-t border-gray-200 px-3 py-2 flex space-x-2">
                <span class="tooltip top" data-tooltip="Open">
                  <a
                    class={["icon-button", app_session.app_status.lifecycle != :active && "disabled"]}
                    aria-label="open app"
                    href={~p"/apps/#{@app.slug}/#{app_session.id}"}
                  >
                    <.remix_icon icon="link" class="text-lg" />
                  </a>
                </span>
                <div class="grow" />
                <span class="tooltip top" data-tooltip="Debug">
                  <a class="icon-button" aria-label="debug app" href={~p"/sessions/#{app_session.id}"}>
                    <.remix_icon icon="terminal-line" class="text-lg" />
                  </a>
                </span>
                <%= if app_session.app_status.lifecycle == :active do %>
                  <span class="tooltip top" data-tooltip="Deactivate">
                    <button
                      class="icon-button"
                      aria-label="deactivate app session"
                      phx-click={
                        JS.push("deactivate_app_session",
                          value: %{session_id: app_session.id},
                          target: @myself
                        )
                      }
                    >
                      <.remix_icon icon="stop-circle-line" class="text-lg" />
                    </button>
                  </span>
                <% else %>
                  <span class="tooltip top" data-tooltip="Terminate">
                    <button
                      class="icon-button"
                      aria-label="terminate app session"
                      phx-click={
                        JS.push("terminate_app_session",
                          value: %{session_id: app_session.id},
                          target: @myself
                        )
                      }
                    >
                      <.remix_icon icon="delete-bin-6-line" class="text-lg" />
                    </button>
                  </span>
                <% end %>
              </div>
            </div>
          </div>
        <% end %>
      <% end %>
    </div>
    """
  end

  defp app_info_icon(assigns) do
    ~H"""
    <span
      class="icon-button p-0 cursor-pointer tooltip bottom-left"
      data-tooltip={
        ~S'''
        App deployment is a way to share your
        notebook for people to interact with. Use
        inputs and controls to build interactive
        UIs, perfect for demos and tasks.
        '''
      }
    >
      <.remix_icon icon="question-line" class="text-xl leading-none" />
    </span>
    """
  end

  @impl true
  def handle_event("terminate_app", %{}, socket) do
    {:noreply, confirm_app_termination(socket, socket.assigns.app.pid)}
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
