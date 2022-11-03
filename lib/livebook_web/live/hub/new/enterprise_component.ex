defmodule LivebookWeb.Hub.New.EnterpriseComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset, only: [get_field: 2]

  alias Livebook.EctoTypes.HexColor
  alias Livebook.Hubs.Enterprise
  alias Livebook.WebSocket

  @app_version Mix.Project.config()[:version]

  @impl true
  def update(assigns, socket) do
    {:ok,
     socket
     |> assign(assigns)
     |> assign(
       base: %Enterprise{},
       changeset: Enterprise.change_hub(%Enterprise{}),
       connected: false
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
        phx-debounce="blur"
      >
        <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
          <.input_wrapper form={f} field={:url} class="flex flex-col space-y-1">
            <div class="input-label">URL</div>
            <%= text_input(f, :url,
              class: "input w-full phx-form-error:border-red-300",
              autofocus: true,
              spellcheck: "false",
              autocomplete: "off"
            ) %>
          </.input_wrapper>

          <.input_wrapper form={f} field={:token} class="flex flex-col space-y-1">
            <div class="input-label">Token</div>
            <%= password_input(f, :token,
              value: get_field(@changeset, :token),
              class: "input w-full phx-form-error:border-red-300",
              spellcheck: "false",
              autocomplete: "off"
            ) %>
          </.input_wrapper>
        </div>

        <button
          id="connect"
          type="button"
          phx-click="connect"
          phx-target={@myself}
          class="button-base button-blue"
        >
          Connect
        </button>

        <%= if @connected do %>
          <div class="grid grid-cols-1 md:grid-cols-1">
            <.input_wrapper form={f} field={:external_id} class="flex flex-col space-y-1">
              <div class="input-label">ID</div>
              <%= text_input(f, :external_id, class: "input", disabled: true) %>
            </.input_wrapper>
          </div>

          <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
            <.input_wrapper form={f} field={:hub_name} class="flex flex-col space-y-1">
              <div class="input-label">Name</div>
              <%= text_input(f, :hub_name, class: "input", readonly: true) %>
            </.input_wrapper>

            <.input_wrapper form={f} field={:hub_color} class="flex flex-col space-y-1">
              <div class="input-label">Color</div>
              <.hex_color_input
                form={f}
                field={:hub_color}
                randomize={JS.push("randomize_color", target: @myself)}
              />
            </.input_wrapper>
          </div>

          <%= submit("Add Hub",
            class: "button-base button-blue",
            phx_disable_with: "Add...",
            disabled: not @changeset.valid?
          ) %>
        <% end %>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("connect", _params, socket) do
    url = get_field(socket.assigns.changeset, :url)
    token = get_field(socket.assigns.changeset, :token)

    case connect_with_enterprise(url, token) do
      {:ok, {:session, session_response}} ->
        base = %Enterprise{
          token: token,
          url: url,
          external_id: session_response.user.id,
          hub_name: "Enterprise",
          hub_color: HexColor.random()
        }

        changeset = Enterprise.change_hub(base)

        {:noreply, assign(socket, changeset: changeset, base: base, connected: true)}

      {:error, reason} ->
        handle_error(reason, socket)
    end
  end

  def handle_event("randomize_color", _, socket) do
    handle_event("validate", %{"enterprise" => %{"hub_color" => HexColor.random()}}, socket)
  end

  def handle_event("save", %{"enterprise" => params}, socket) do
    if socket.assigns.changeset.valid? do
      case Enterprise.create_hub(socket.assigns.base, params) do
        {:ok, hub} ->
          {:noreply,
           socket
           |> put_flash(:success, "Hub added successfully")
           |> push_redirect(to: Routes.hub_path(socket, :edit, hub.id))}

        {:error, changeset} ->
          {:noreply, assign(socket, changeset: changeset)}
      end
    else
      {:noreply, socket}
    end
  end

  def handle_event("validate", %{"enterprise" => attrs}, socket) do
    {:noreply, assign(socket, changeset: Enterprise.change_hub(socket.assigns.base, attrs))}
  end

  defp connect_with_enterprise(url, token) do
    headers = [{"X-Auth-Token", token}]
    session_request = LivebookProto.SessionRequest.new!(app_version: @app_version)

    with {:ok, connection, :connected} <- WebSocket.connect(url, headers),
         {:ok, connection} <- WebSocket.send_request(connection, session_request),
         {:ok, connection, session_response} <- WebSocket.receive_response(connection) do
      WebSocket.disconnect(connection)
      {:ok, session_response}
    else
      {:error, connection, response} ->
        WebSocket.disconnect(connection)
        {:error, response}
    end
  end

  def handle_error(%{reason: :econnrefused}, socket) do
    show_connect_error("Failed to connect with given URL", socket)
  end

  def handle_error(%{details: reason}, socket) do
    show_connect_error(reason, socket)
  end

  defp show_connect_error(message, socket) do
    {:noreply,
     socket
     |> put_flash(:error, message)
     |> push_patch(to: Routes.hub_path(socket, :new))}
  end
end
