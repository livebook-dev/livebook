defmodule LivebookWeb.SessionLive.SecretsListComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col grow">
      <div class="flex justify-between items-center">
        <h3 class="uppercase text-sm font-semibold text-gray-500">
          Secrets
        </h3>
        <.secrets_info_icon />
      </div>
      <span class="text-sm text-gray-500">Available only to this session</span>
      <div class="flex flex-col">
        <div class="flex flex-col space-y-4 mt-6">
          <.session_secret
            :for={
              secret <- @secrets |> Session.Data.session_secrets(@hub.id) |> Enum.sort_by(& &1.name)
            }
            id={"session-secret-#{secret.name}"}
            secret={secret}
            myself={@myself}
          />
        </div>

        <.link
          id="new-secret-button"
          patch={~p"/sessions/#{@session.id}/secrets"}
          class="inline-flex items-center justify-center p-8 py-1 mt-6 space-x-2 text-sm font-medium text-gray-500 border border-gray-400 border-dashed rounded-xl hover:bg-gray-100"
          role="button"
        >
          <.remix_icon icon="add-line" class="text-lg align-center" />
          <span>New secret</span>
        </.link>

        <div class="mt-16">
          <h3 class="uppercase text-sm font-semibold text-gray-500">
            <%= @hub.hub_emoji %> <%= @hub.hub_name %> secrets
          </h3>
          <span class="text-sm text-gray-500">
            <%= if @hub_secrets == [] do %>
              No secrets stored in this hub so far
            <% else %>
              Toggle to share with this session
            <% end %>
          </span>
        </div>

        <div class="flex flex-col space-y-4 mt-6">
          <.hub_secret
            :for={secret <- Enum.sort_by(@hub_secrets, & &1.name)}
            id={"hub-#{secret.hub_id}-secret-#{secret.name}"}
            secret={secret}
            secrets={@secrets}
            hub={@hub}
            myself={@myself}
          />
        </div>
      </div>
    </div>
    """
  end

  defp session_secret(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col text-gray-500 rounded-lg px-2 pt-1">
      <span
        class="text-sm font-mono break-all flex-row cursor-pointer"
        phx-click={
          JS.toggle(to: "#session-secret-#{@secret.name}-detail", display: "flex")
          |> toggle_class("bg-gray-100", to: "#session-secret-#{@secret.name}")
        }
      >
        <%= @secret.name %>
      </span>
      <div
        class="flex justify-between items-center my-1 hidden"
        id={"session-secret-#{@secret.name}-detail"}
      >
        <div class="flex items-center gap-1">
          <span class="text-sm font-mono break-all">
            *****
          </span>
          <button
            id={"session-secret-#{@secret.name}-delete"}
            type="button"
            phx-click={JS.dispatch("lb:clipcopy", detail: %{content: @secret.value})}
            class="icon-button"
          >
            <.remix_icon icon="clipboard-line" />
          </button>
        </div>
        <button
          id={"session-secret-#{@secret.name}-delete"}
          type="button"
          phx-click={
            JS.push("delete_session_secret",
              value: %{secret_name: @secret.name},
              target: @myself
            )
          }
          class="icon-button"
        >
          <.remix_icon icon="delete-bin-line" />
        </button>
      </div>
    </div>
    """
  end

  defp hub_secret(assigns) do
    ~H"""
    <div id={@id} class="flex flex-col text-gray-500 rounded-lg px-2 pt-1">
      <div class="flex flex-col text-gray-800">
        <div class="flex flex-col">
          <div class="flex justify-between items-center">
            <span
              class="text-sm font-mono w-full break-all flex-row cursor-pointer"
              phx-click={
                JS.toggle(to: "##{@id}-detail", display: "flex")
                |> toggle_class("bg-gray-100", to: "##{@id}")
              }
            >
              <%= @secret.name %>
            </span>
            <span
              class="mr-2 tooltip bottom-left"
              data-tooltip={
                ~S'''
                The secret value changed,
                click to load the latest one.
                '''
              }
            >
              <button
                :if={Session.Data.secret_outdated?(@secret, @secrets)}
                class="icon-button"
                aria-label="load latest value"
                phx-click={
                  JS.push("update_outdated", value: %{"name" => @secret.name}, target: @myself)
                }
              >
                <.remix_icon icon="refresh-line" class="text-xl leading-none" />
              </button>
            </span>
            <.form
              :let={f}
              id={"#{@id}-toggle"}
              for={%{"toggled" => Session.Data.secret_toggled?(@secret, @secrets)}}
              as={:data}
              phx-change="toggle_secret"
              phx-target={@myself}
            >
              <.switch_field field={f[:toggled]} />
              <.hidden_field field={f[:name]} value={@secret.name} />
            </.form>
          </div>
          <div class="flex-row justify-between items-center my-1 hidden" id={"#{@id}-detail"}>
            <div class="flex items-center gap-1">
              <span class="text-sm font-mono break-all">
                *****
              </span>
              <button
                id={"session-secret-#{@secret.name}-delete"}
                type="button"
                phx-click={JS.dispatch("lb:clipcopy", detail: %{content: @secret.value})}
                class="icon-button"
              >
                <.remix_icon icon="clipboard-line" />
              </button>
            </div>
            <.link
              id={"#{@id}-edit-button"}
              navigate={~p"/hub/#{@secret.hub_id}/secrets/edit/#{@secret.name}"}
              class="icon-button"
              role="button"
            >
              <.remix_icon icon="pencil-line" />
            </.link>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp secrets_info_icon(assigns) do
    ~H"""
    <span
      class="icon-button p-0 cursor-pointer tooltip bottom-left"
      data-tooltip={
        ~S'''
        Secrets are a safe way to share credentials
        and tokens with notebooks. They are often
        shared with Smart cells and can be read as
        environment variables using the LB_ prefix.
        '''
      }
    >
      <.remix_icon icon="question-line" class="text-xl leading-none" />
    </span>
    """
  end

  @impl true
  def handle_event("toggle_secret", %{"data" => data}, socket) do
    if data["toggled"] == "true" do
      secret = Enum.find(socket.assigns.hub_secrets, &(&1.name == data["name"]))
      Session.set_secret(socket.assigns.session.pid, secret)
    else
      Session.unset_secret(socket.assigns.session.pid, data["name"])
    end

    {:noreply, socket}
  end

  def handle_event("update_outdated", %{"name" => name}, socket) do
    secret = Enum.find(socket.assigns.hub_secrets, &(&1.name == name))
    Session.set_secret(socket.assigns.session.pid, secret)

    {:noreply, socket}
  end

  def handle_event("delete_session_secret", %{"secret_name" => secret_name}, socket) do
    on_confirm = fn socket ->
      Session.unset_secret(socket.assigns.session.pid, secret_name)
      socket
    end

    {:noreply,
     confirm(socket, on_confirm,
       title: "Delete session secret - #{secret_name}",
       description: "Are you sure you want to delete this session secret?",
       confirm_text: "Delete",
       confirm_icon: "delete-bin-6-line"
     )}
  end
end
