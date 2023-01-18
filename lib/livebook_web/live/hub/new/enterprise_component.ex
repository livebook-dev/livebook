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
          <.input_wrapper form={f} field={:url} class="flex flex-col space-y-1">
            <div class="input-label">URL</div>
            <%= text_input(f, :url,
              class: "input w-full phx-form-error:border-red-300",
              autofocus: true,
              spellcheck: "false",
              autocomplete: "off",
              phx_debounce: "blur"
            ) %>
          </.input_wrapper>

          <.input_wrapper form={f} field={:token} class="flex flex-col space-y-1">
            <div class="input-label">Token</div>
            <%= password_input(f, :token,
              value: get_field(@changeset, :token),
              class: "input w-full phx-form-error:border-red-300",
              spellcheck: "false",
              autocomplete: "off",
              phx_debounce: "blur"
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

        <%= if @pid do %>
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

            <.input_wrapper form={f} field={:hub_emoji} class="flex flex-col space-y-1">
              <div class="input-label">Emoji</div>
              <.emoji_input id="enterprise-emoji-input" form={f} field={:hub_emoji} />
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
        EnterpriseClient.stop(pid)

        {:noreply,
         socket
         |> put_flash(:error, "Failed to connect with Enterprise: " <> reason)
         |> push_patch(to: Routes.hub_path(socket, :new))}

      :hub_connected ->
        session_request =
          LivebookProto.SessionRequest.new!(app_version: Livebook.Config.app_version())

        case EnterpriseClient.send_request(pid, session_request) do
          {:session, session_response} ->
            base = %{base | external_id: session_response.id}
            changeset = Enterprise.change_hub(base)

            {:noreply, assign(socket, pid: pid, changeset: changeset, base: base)}

          {:error, reason} ->
            EnterpriseClient.stop(pid)

            {:noreply,
             socket
             |> put_flash(:error, "Failed to connect with Enterprise: " <> reason)
             |> push_patch(to: Routes.hub_path(socket, :new))}
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
end
