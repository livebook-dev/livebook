defmodule LivebookWeb.SessionLive.SaveRuntimeConfigComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  @impl true
  def mount(socket) do
    {:ok, assign(socket, save_config: nil)}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      case {socket.assigns.save_config_payload, socket.assigns.save_config} do
        {nil, nil} ->
          socket

        {_, nil} ->
          deafult_name = socket.assigns.secret_prefix
          changeset = config_secret_changeset(socket, %{name: deafult_name})
          save_config = %{changeset: changeset, inflight: false, error: false}
          assign(socket, save_config: save_config)

        {nil, _} ->
          assign(socket, save_config: nil)

        {_, _} ->
          socket
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <%= if @save_config do %>
        <.save_config_form save_config={@save_config} hub={@hub} myself={@myself} />
      <% else %>
        <.config_actions secret_prefix={@secret_prefix} hub_secrets={@hub_secrets} myself={@myself} />
      <% end %>
    </div>
    """
  end

  defp config_actions(assigns) do
    ~H"""
    <div class="mt-1 flex justify-end gap-1">
      <.button
        color="gray"
        outlined
        small
        type="button"
        phx-click="open_save_config"
        phx-target={@myself}
      >
        Save config
      </.button>
      <.menu id="config-secret-menu">
        <:toggle>
          <.button color="gray" outlined small type="button">
            <span>Load config</span>
            <.remix_icon icon="arrow-down-s-line" class="text-base leading-none" />
          </.button>
        </:toggle>
        <div
          :if={config_secret_names(@hub_secrets, @secret_prefix) == []}
          class="px-3 py-1 whitespace-nowrap text-gray-600 text-sm"
        >
          No configs saved yet
        </div>
        <.menu_item :for={name <- config_secret_names(@hub_secrets, @secret_prefix)}>
          <button
            class="text-gray-500 text-sm"
            type="button"
            role="menuitem"
            phx-click={JS.push("load_config", value: %{name: name}, target: @myself)}
          >
            {name}
          </button>
        </.menu_item>
      </.menu>
    </div>
    """
  end

  defp save_config_form(assigns) do
    ~H"""
    <.form
      :let={f}
      for={@save_config.changeset}
      as={:secret}
      class="mt-4 flex flex-col"
      phx-change="validate_save_config"
      phx-submit="save_config"
      phx-target={@myself}
      autocomplete="off"
      spellcheck="false"
    >
      <div class="text-lg text-gray-800 font-semibold">
        Save config
      </div>
      <div class="mt-1 text-gray-700">
        Store the config in a secret in the <.workspace hub={@hub} /> workspace to reuse it later.
      </div>
      <div :if={error = @save_config.error} class="mt-4">
        <.message_box kind="error" message={error} />
      </div>
      <div class="mt-4 grid grid-cols-3">
        <.text_field field={f[:name]} label="Secret name" class="uppercase" autofocus />
      </div>
      <div class="mt-6 flex gap-2">
        <.button type="submit" disabled={not @save_config.changeset.valid? or @save_config.inflight}>
          {if(@save_config.inflight, do: "Saving...", else: "Save")}
        </.button>
        <.button
          color="gray"
          outlined
          type="button"
          phx-click="cancel_save_config"
          phx-target={@myself}
        >
          Cancel
        </.button>
      </div>
    </.form>
    """
  end

  defp workspace(assigns) do
    ~H"""
    <span class="font-medium">
      <span class="text-lg">{@hub.hub_emoji}</span>
      <span>{@hub.hub_name}</span>
    </span>
    """
  end

  @impl true
  def handle_event("open_save_config", %{}, socket) do
    send_event(socket.assigns.target, :open_save_config)
    {:noreply, socket}
  end

  def handle_event("cancel_save_config", %{}, socket) do
    send_event(socket.assigns.target, :close_save_config)
    {:noreply, socket}
  end

  def handle_event("validate_save_config", %{"secret" => secret}, socket) do
    changeset =
      socket
      |> config_secret_changeset(secret)
      |> Map.replace!(:action, :validate)

    {:noreply, assign_nested(socket, :save_config, changeset: changeset)}
  end

  def handle_event("save_config", %{"secret" => secret}, socket) do
    changeset = config_secret_changeset(socket, secret)

    case Ecto.Changeset.apply_action(changeset, :insert) do
      {:ok, secret} ->
        {:noreply, save_config_secret(socket, secret, changeset)}

      {:error, changeset} ->
        {:noreply, assign_nested(socket, :save_config, changeset: changeset)}
    end
  end

  def handle_event("load_config", %{"name" => name}, socket) do
    secret = Enum.find(socket.assigns.hub_secrets, &(&1.name == name))

    case JSON.decode(secret.value) do
      {:ok, config_defaults} ->
        send_event(socket.assigns.target, {:load_config, config_defaults})
        {:noreply, socket}

      {:error, _} ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_async(:save_config, {:ok, result}, socket) do
    socket =
      case result do
        :ok ->
          send_event(socket.assigns.target, :close_save_config)
          assign_nested(socket, :save_config, inflight: false)

        {:error, %Ecto.Changeset{} = changeset} ->
          assign_nested(socket, :save_config, changeset: changeset, inflight: false)

        {:transport_error, error} ->
          assign_nested(socket, :save_config, error: error, inflight: false)
      end

    {:noreply, socket}
  end

  defp config_secret_names(hub_secrets, secret_prefix) do
    names =
      for %{name: name} <- hub_secrets,
          String.starts_with?(name, secret_prefix),
          do: name

    Enum.sort(names)
  end

  defp config_secret_changeset(socket, attrs) do
    secret_prefix = socket.assigns.secret_prefix
    hub = socket.assigns.hub
    value = JSON.encode!(socket.assigns.save_config_payload)
    secret = %Livebook.Secrets.Secret{hub_id: hub.id, name: nil, value: value}

    secret
    |> Livebook.Secrets.change_secret(attrs)
    |> validate_format(:name, ~r/^#{secret_prefix}\w+$/,
      message: "must be in the format #{secret_prefix}*"
    )
  end

  defp save_config_secret(socket, secret, changeset) do
    hub = socket.assigns.hub
    exists? = Enum.any?(socket.assigns.hub_secrets, &(&1.name == secret.name))

    socket
    |> start_async(:save_config, fn ->
      result =
        if exists? do
          Livebook.Hubs.update_secret(hub, secret)
        else
          Livebook.Hubs.create_secret(hub, secret)
        end

      with {:error, errors} <- result do
        {:error,
         changeset
         |> Livebook.Utils.put_changeset_errors(errors)
         |> Map.replace!(:action, :validate)}
      end
    end)
    |> assign_nested(:save_config, inflight: true)
  end
end
