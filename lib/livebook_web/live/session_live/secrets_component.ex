defmodule LivebookWeb.SessionLive.SecretsComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs.EnterpriseClient

  @impl true
  def update(assigns, socket) do
    socket =
      socket
      |> assign(assigns)
      |> assign(connected_hubs: Livebook.Hubs.get_connected_hubs())

    prefill_form = prefill_secret_name(socket)

    socket =
      if socket.assigns[:data] do
        socket
      else
        assign(socket,
          data: %{"name" => prefill_form, "value" => "", "store" => "session"},
          errors: [{"value", {"can't be blank", []}}],
          title: title(socket),
          grant_access: must_grant_access(socket),
          has_prefill: prefill_form != ""
        )
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 max-w-4xl flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        <%= @title %>
      </h3>
      <%= if @grant_access do %>
        <.grant_access_message grant_access={@grant_access} target={@myself} />
      <% end %>
      <div class="flex flex-columns gap-4">
        <%= if @select_secret_ref do %>
          <div class="basis-1/2 grow-0 pr-4 border-r">
            <div class="flex flex-col space-y-4">
              <p class="text-gray-800">
                Choose a secret
              </p>
              <div class="flex flex-wrap">
                <%= for {secret_name, _} <- Enum.sort(@secrets) do %>
                  <.secret_with_badge
                    secret_name={secret_name}
                    stored="Session"
                    action="select_secret"
                    active={secret_name == @prefill_secret_name}
                    target={@myself}
                  />
                <% end %>
                <%= for {secret_name, _} <- livebook_only_secrets(@secrets, @livebook_secrets) do %>
                  <.secret_with_badge
                    secret_name={secret_name}
                    stored="Livebook"
                    action="select_livebook_secret"
                    active={false}
                    target={@myself}
                  />
                <% end %>
                <%= if @secrets == %{} and @livebook_secrets == %{} do %>
                  <div class="w-full text-center text-gray-400 border rounded-lg p-8">
                    <.remix_icon icon="folder-lock-line" class="align-middle text-2xl" />
                    <span class="mt-1 block text-sm text-gray-700">
                      Secrets not found. <br /> Add to see them here.
                    </span>
                  </div>
                <% end %>
              </div>
            </div>
          </div>
        <% end %>
        <.form
          :let={f}
          for={:data}
          phx-submit="save"
          phx-change="validate"
          autocomplete="off"
          phx-target={@myself}
          errors={@errors}
          class="basis-1/2 grow"
        >
          <div class="flex flex-col space-y-4">
            <%= if @select_secret_ref do %>
              <p class="text-gray-700">
                Add new secret
              </p>
            <% end %>
            <.input_wrapper form={f} field={:name}>
              <div class="input-label">
                Name <span class="text-xs text-gray-500">(alphanumeric and underscore)</span>
              </div>
              <%= text_input(f, :name,
                value: @data["name"],
                class: "input",
                autofocus: !@has_prefill,
                spellcheck: "false"
              ) %>
            </.input_wrapper>
            <.input_wrapper form={f} field={:value}>
              <div class="input-label">Value</div>
              <%= text_input(f, :value,
                value: @data["value"],
                class: "input",
                autofocus: @has_prefill,
                spellcheck: "false"
              ) %>
            </.input_wrapper>
            <div>
              <div class="input-label">Storage</div>
              <div class="my-2 space-y-1 text-sm">
                <%= label  class: "flex items-center gap-2 text-gray-600" do %>
                  <%= radio_button(f, :store, "session", checked: @data["store"] == "session") %> only this session
                <% end %>
                <%= label class: "flex items-center gap-2 text-gray-600" do %>
                  <%= radio_button(f, :store, "livebook", checked: @data["store"] == "livebook") %> in the Livebook app
                <% end %>
                <%= if Livebook.Config.feature_flag_enabled?(:hub) do %>
                  <%= label class: "flex items-center gap-2 text-gray-600" do %>
                    <%= radio_button(f, :store, "hub",
                      disabled: @connected_hubs == [],
                      checked: @data["store"] == "hub"
                    ) %> in the Hub
                  <% end %>
                  <%= if @data["store"] == "hub" do %>
                    <%= select(
                      f,
                      :connected_hub,
                      connected_hubs_options(@connected_hubs, @data["connected_hub"]),
                      class: "input"
                    ) %>
                  <% end %>
                <% end %>
              </div>
            </div>
            <div class="flex space-x-2">
              <button class="button-base button-blue" type="submit" disabled={f.errors != []}>
                <.remix_icon icon="add-line" class="align-middle" />
                <span class="font-normal">Add</span>
              </button>
              <%= live_patch("Cancel", to: @return_to, class: "button-base button-outlined-gray") %>
            </div>
          </div>
        </.form>
      </div>
    </div>
    """
  end

  defp secret_with_badge(assigns) do
    ~H"""
    <div
      role="button"
      class={[
        "flex justify-between w-full font-mono text-sm p-2 border-b cursor-pointer",
        if @active do
          "bg-blue-100 text-blue-700"
        else
          "text-gray-700 hover:bg-gray-100"
        end
      ]}
      phx-value-secret_name={@secret_name}
      phx-target={@target}
      phx-click={@action}
    >
      <%= @secret_name %>
      <span class={[
        "inline-flex items-center font-sans rounded-full px-2.5 py-0.5 text-xs font-medium bg-gray-100",
        if @active do
          "bg-indigo-100 text-blue-800"
        else
          "bg-gray-100 text-gray-800"
        end
      ]}>
        <%= if @active do %>
          <svg class="-ml-0.5 mr-1.5 h-2 w-2 text-blue-400" fill="currentColor" viewBox="0 0 8 8">
            <circle cx="4" cy="4" r="3" />
          </svg>
        <% end %>
        <%= @stored %>
      </span>
    </div>
    """
  end

  defp grant_access_message(assigns) do
    ~H"""
    <div>
      <div class="mx-auto">
        <div class="rounded-lg bg-blue-600 py-1 px-4 shadow-sm">
          <div class="flex flex-wrap items-center justify-between">
            <div class="flex w-0 flex-1 items-center">
              <.remix_icon
                icon="error-warning-fill"
                class="align-middle text-2xl flex text-gray-100 rounded-lg py-2"
              />
              <span class="ml-2 text-sm font-normal text-gray-100">
                There is a secret named
                <span class="font-semibold text-white"><%= @grant_access %></span>
                in your Livebook app. Allow this session to access it?
              </span>
            </div>
            <button
              class="button-base button-gray"
              phx-click="grant_access"
              phx-value-secret_name={@grant_access}
              phx-target={@target}
            >
              Grant access
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"data" => data}, socket) do
    with {:ok, secret} <- Livebook.Secrets.validate_secret(data),
         :ok <- set_secret(socket, secret, data["store"]) do
      {:noreply,
       socket
       |> push_patch(to: socket.assigns.return_to)
       |> push_secret_selected(secret.name)}
    else
      {:error, %{errors: errors}} ->
        {:noreply, assign(socket, errors: errors)}

      {:error, socket} ->
        {:noreply, socket}
    end
  end

  def handle_event("select_secret", %{"secret_name" => secret_name}, socket) do
    {:noreply,
     socket |> push_patch(to: socket.assigns.return_to) |> push_secret_selected(secret_name)}
  end

  def handle_event("select_livebook_secret", %{"secret_name" => secret_name}, socket) do
    grant_access(secret_name, socket)

    {:noreply,
     socket |> push_patch(to: socket.assigns.return_to) |> push_secret_selected(secret_name)}
  end

  def handle_event("validate", %{"data" => data}, socket) do
    socket = assign(socket, data: data)

    case Livebook.Secrets.validate_secret(data) do
      {:ok, _} -> {:noreply, assign(socket, errors: [])}
      {:error, changeset} -> {:noreply, assign(socket, errors: changeset.errors)}
    end
  end

  def handle_event("grant_access", %{"secret_name" => secret_name}, socket) do
    grant_access(secret_name, socket)

    {:noreply,
     socket |> push_patch(to: socket.assigns.return_to) |> push_secret_selected(secret_name)}
  end

  defp push_secret_selected(%{assigns: %{select_secret_ref: nil}} = socket, _), do: socket

  defp push_secret_selected(%{assigns: %{select_secret_ref: ref}} = socket, secret_name) do
    push_event(socket, "secret_selected", %{select_secret_ref: ref, secret_name: secret_name})
  end

  defp prefill_secret_name(socket) do
    if unavailable_secret?(
         socket.assigns.prefill_secret_name,
         socket.assigns.secrets,
         socket.assigns.livebook_secrets
       ),
       do: socket.assigns.prefill_secret_name,
       else: ""
  end

  defp unavailable_secret?(nil, _, _), do: false
  defp unavailable_secret?("", _, _), do: false

  defp unavailable_secret?(preselect_name, secrets, livebook_secrets) do
    not Map.has_key?(secrets, preselect_name) and
      not Map.has_key?(livebook_secrets, preselect_name)
  end

  defp title(%{assigns: %{select_secret_ref: nil}}), do: "Add secret"
  defp title(%{assigns: %{select_secret_options: %{"title" => title}}}), do: title
  defp title(_), do: "Select secret"

  defp set_secret(socket, secret, "session") do
    secret = %{secret | origin: :system_env}
    Livebook.Session.set_secret(socket.assigns.session.pid, secret)
  end

  defp set_secret(socket, secret, "livebook") do
    secret = %{secret | origin: :app}
    Livebook.Secrets.set_secret(secret)
    Livebook.Session.set_secret(socket.assigns.session.pid, secret)
  end

  defp set_secret(socket, secret, "hub") do
    selected_hub = socket.assigns.data["connected_hub"]

    if hub = Enum.find(socket.assigns.connected_hubs, &(&1.hub.id == selected_hub)) do
      create_secret_request =
        LivebookProto.CreateSecretRequest.new!(
          name: secret.name,
          value: secret.value
        )

      case EnterpriseClient.send_request(hub.pid, create_secret_request) do
        {:create_secret, _} -> :ok
        {:error, reason} -> {:error, put_flash(socket, :error, reason)}
      end
    else
      {:error, %{errors: [{"connected_hub", {"can't be blank", []}}]}}
    end
  end

  defp grant_access(secret_name, socket) do
    secret_value = socket.assigns.livebook_secrets[secret_name]
    secret = %{name: secret_name, value: secret_value}
    set_secret(socket.assigns.session.pid, secret, "session")
  end

  defp livebook_only_secrets(secrets, livebook_secrets) do
    Enum.reject(livebook_secrets, &(&1 in secrets)) |> Enum.sort()
  end

  defp must_grant_access(%{assigns: %{prefill_secret_name: prefill_secret_name}} = socket) do
    if not Map.has_key?(socket.assigns.secrets, prefill_secret_name) and
         Map.has_key?(socket.assigns.livebook_secrets, prefill_secret_name) do
      prefill_secret_name
    end
  end

  # TODO: Livebook.Hubs.fetch_hubs_with_secrets_storage()
  defp connected_hubs_options(connected_hubs, selected_hub) do
    [[key: "Select one Hub", value: "", selected: true, disabled: true]] ++
      for %{hub: %{id: id, hub_name: name}} <- connected_hubs do
        [key: name, value: id, selected: id == selected_hub]
      end
  end
end
