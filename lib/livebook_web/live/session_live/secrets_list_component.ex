defmodule LivebookWeb.SessionLive.SecretsListComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session

  @impl true
  def mount(socket) do
    {:ok, assign(socket, hub_secrets_counter: 1)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col grow" data-el-secrets-list>
      <div class="flex justify-between items-center">
        <h3 class="uppercase text-sm font-semibold text-gray-500">
          Secrets
        </h3>
        <.secrets_info_icon />
      </div>
      <span class="text-sm text-gray-500">Available only to this session</span>
      <div class="flex flex-col">
        <div
          :if={Session.Data.session_secrets(@secrets, @hub.id) != []}
          class="flex flex-col space-y-4 mt-6"
        >
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
            {@hub.hub_emoji} {@hub.hub_name} secrets
          </h3>
          <span class="text-sm text-gray-500">
            <%= if @hub_secrets == [] do %>
              No secrets stored in this workspace so far
            <% else %>
              Toggle to allow access to a secret
            <% end %>
          </span>
        </div>

        <div class="flex flex-col space-y-4 mt-6" id={"hub-secrets-#{@hub_secrets_counter}"}>
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
          |> JS.toggle_class("bg-gray-100", to: "#session-secret-#{@secret.name}")
        }
      >
        {@secret.name}
      </span>
      <div
        class="flex justify-between items-center my-1 hidden"
        id={"session-secret-#{@secret.name}-detail"}
      >
        <div class="flex items-center gap-1">
          <span class="text-sm font-mono break-all">
            *****
          </span>
          <.icon_button
            small
            id={"session-secret-#{@secret.name}-copy"}
            type="button"
            phx-click={JS.dispatch("lb:clipcopy", detail: %{content: @secret.value})}
          >
            <.remix_icon icon="clipboard-line" />
          </.icon_button>
        </div>
        <.icon_button
          small
          id={"session-secret-#{@secret.name}-delete"}
          type="button"
          phx-click={
            JS.push("delete_session_secret",
              value: %{secret_name: @secret.name},
              target: @myself
            )
          }
        >
          <.remix_icon icon="delete-bin-line" />
        </.icon_button>
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
                |> JS.toggle_class("bg-gray-100", to: "##{@id}")
              }
            >
              {@secret.name}
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
              <.icon_button
                :if={Session.Data.secret_outdated?(@secret, @secrets)}
                aria-label="load latest value"
                phx-click={
                  JS.push("update_outdated", value: %{"name" => @secret.name}, target: @myself)
                }
              >
                <.remix_icon icon="refresh-line" />
              </.icon_button>
            </span>
            <.form
              :let={f}
              id={"#{@id}-toggle"}
              for={%{"toggled" => Session.Data.secret_toggled?(@secret, @secrets)}}
              as={:data}
              phx-change="toggle_secret"
              phx-target={@myself}
            >
              <.switch_field
                field={f[:toggled]}
                value={Session.Data.secret_toggled?(@secret, @secrets)}
              />
              <.hidden_field field={f[:name]} value={@secret.name} />
            </.form>
          </div>
          <div class="flex-row justify-between items-center my-1 hidden" id={"#{@id}-detail"}>
            <div class="flex items-center gap-1">
              <span class="text-sm font-mono break-all">
                *****
              </span>
              <.icon_button
                small
                id={"#{@id}-copy-button"}
                type="button"
                phx-click={JS.dispatch("lb:clipcopy", detail: %{content: @secret.value})}
              >
                <.remix_icon icon="clipboard-line" />
              </.icon_button>
            </div>
            <.icon_button
              small
              id={"#{@id}-edit-button"}
              navigate={~p"/hub/#{@secret.hub_id}/secrets/edit/#{@secret.name}"}
              role="button"
            >
              <.remix_icon icon="pencil-line" />
            </.icon_button>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp secrets_info_icon(assigns) do
    ~H"""
    <span
      class="tooltip bottom-left"
      data-tooltip={
        ~S'''
        Secrets are a safe way to allow notebooks
        to access credentials and tokens.
        Your notebook can read them as environment
        variables using the LB_ prefix.
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
  def handle_event("toggle_secret", %{"data" => data}, socket) do
    if data["toggled"] == "true" do
      secret = Enum.find(socket.assigns.hub_secrets, &(&1.name == data["name"]))

      session_secrets =
        Session.Data.session_secrets(socket.assigns.secrets, socket.assigns.hub.id)

      overrides_session_secret? = Enum.any?(session_secrets, &(&1.name == secret.name))

      if overrides_session_secret? do
        session_pid = socket.assigns.session.pid

        on_confirm = fn socket ->
          Session.set_secret(session_pid, secret)
          socket
        end

        {:noreply,
         socket
         # Before the action is confirmed, we want to render the secret
         # back as "untoggled", so we force a patch to override the
         # client state
         |> update(:hub_secrets_counter, &(&1 + 1))
         |> confirm(on_confirm,
           title: "Override session secret",
           description:
             "There is a session secret named #{secret.name}, this action will erase it.",
           confirm_text: "Override"
         )}
      else
        Session.set_secret(socket.assigns.session.pid, secret)
        {:noreply, socket}
      end
    else
      Session.unset_secret(socket.assigns.session.pid, data["name"])
      {:noreply, socket}
    end
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
       title: "Delete session secret",
       description: "Are you sure you want to delete #{secret_name} session secret?",
       confirm_text: "Delete",
       confirm_icon: "delete-bin-6-line"
     )}
  end
end
