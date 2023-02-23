defmodule LivebookWeb.Hub.New.EnterpriseComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset, only: [get_field: 2]

  alias Livebook.Hubs.{Enterprise, EnterpriseClient}

  @impl true
  def update(assigns, socket) do
    if connected?(socket) do
      Livebook.Hubs.subscribe(:connection)
    end

    {:ok,
     socket
     |> assign(assigns)
     |> assign(
       base: %Enterprise{},
       changeset: Enterprise.change_hub(%Enterprise{}),
       pid: nil
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.form
        :let={f}
        id={@id}
        class="flex flex-col space-y-4"
        for={@changeset}
        phx-submit="save"
        phx-change="validate"
        phx-target={@myself}
      >
        <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
          <.text_field
            field={f[:url]}
            label="URL"
            autofocus
            spellcheck="false"
            autocomplete="off"
            phx-debounce="blur"
          />
          <.password_field
            type="password"
            field={f[:token]}
            label="Token"
            spellcheck="false"
            autocomplete="off"
            phx-debounce="blur"
          />
        </div>

        <div>
          <button
            id="connect"
            type="button"
            phx-click="connect"
            phx-target={@myself}
            class="button-base button-blue"
          >
            Connect
          </button>
        </div>

        <%= if @pid do %>
          <div class="grid grid-cols-1 md:grid-cols-1">
            <.password_field type="password" field={f[:external_id]} label="ID" disabled />
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
            <.text_field field={f[:hub_name]} label="Name" readonly />
            <.emoji_field field={f[:hub_emoji]} label="Emoji" />
          </div>

          <div>
            <button
              class="button-base button-blue"
              type="submit"
              phx-disable-with="Add..."
              disabled={not @changeset.valid?}
            >
              Add Hub
            </button>
          </div>
        <% end %>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("connect", _params, socket) do
    url = get_field(socket.assigns.changeset, :url)
    token = get_field(socket.assigns.changeset, :token)

    base = %Enterprise{
      id: "enterprise-placeholder",
      token: token,
      external_id: "placeholder",
      url: url,
      hub_name: "Enterprise",
      hub_emoji: "🏭"
    }

    {:ok, pid} = EnterpriseClient.start_link(base)

    receive do
      {:hub_connection_failed, reason} ->
        EnterpriseClient.stop(base.id)

        {:noreply,
         socket
         |> put_flash(:error, "Failed to connect with Enterprise: " <> reason)
         |> push_patch(to: ~p"/hub")}

      :hub_connected ->
        data = LivebookProto.build_handshake_request(app_version: Livebook.Config.app_version())

        case EnterpriseClient.send_request(pid, data) do
          {:handshake, handshake_response} ->
            base = %{base | external_id: handshake_response.id}
            changeset = Enterprise.validate_hub(base)

            {:noreply, assign(socket, pid: pid, changeset: changeset, base: base)}

          {:transport_error, reason} ->
            EnterpriseClient.stop(base.id)

            {:noreply,
             socket
             |> put_flash(:error, "Failed to connect with Enterprise: " <> reason)
             |> push_patch(to: ~p"/hub")}
        end
    end
  end

  def handle_event("save", %{"enterprise" => params}, socket) do
    if socket.assigns.changeset.valid? do
      case Enterprise.create_hub(socket.assigns.base, params) do
        {:ok, hub} ->
          if pid = socket.assigns.pid do
            GenServer.stop(pid)
          end

          {:noreply,
           socket
           |> put_flash(:success, "Hub added successfully")
           |> push_navigate(to: ~p"/hub/#{hub.id}")}

        {:error, changeset} ->
          {:noreply, assign(socket, changeset: changeset)}
      end
    else
      {:noreply, socket}
    end
  end

  def handle_event("validate", %{"enterprise" => attrs}, socket) do
    {:noreply, assign(socket, changeset: Enterprise.validate_hub(socket.assigns.base, attrs))}
  end
end
