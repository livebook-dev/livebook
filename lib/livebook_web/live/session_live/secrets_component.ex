defmodule LivebookWeb.SessionLive.SecretsComponent do
  use LivebookWeb, :live_component

  alias Livebook.Hubs
  alias Livebook.Secrets
  alias Livebook.Secrets.Secret
  alias Livebook.Session

  @impl true
  def mount(socket) do
    {:ok, assign(socket, title: title(socket))}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    secret_name = socket.assigns[:prefill_secret_name]

    socket =
      socket
      |> assign_new(:changeset, fn ->
        attrs = %{name: secret_name, value: nil, hub_id: nil}
        Secrets.change_secret(%Secret{}, attrs)
      end)
      |> assign_new(:grant_access_secret, fn ->
        Enum.find(
          socket.assigns.hub_secrets,
          &(&1.name == secret_name and not is_map_key(socket.assigns.secrets, secret_name))
        )
      end)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 w-full flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        <%= @title %>
      </h3>
      <.grant_access_message
        :if={@grant_access_secret}
        secret={@grant_access_secret}
        target={@myself}
        hub={@hub}
      />
      <div class="flex flex-columns gap-4">
        <div :if={@select_secret_ref} class="basis-1/2 grow-0 pr-4 border-r">
          <div class="flex flex-col space-y-4">
            <p class="text-gray-800">
              Choose a secret
            </p>
            <div class="flex flex-wrap">
              <.secret_with_badge
                :for={
                  secret <-
                    @secrets |> Session.Data.session_secrets(@hub.id) |> Enum.sort_by(& &1.name)
                }
                secret_name={secret.name}
                hub?={false}
                stored="Session"
                active={secret.name == @prefill_secret_name}
                target={@myself}
              />
              <.secret_with_badge
                :for={secret <- Enum.sort_by(@hub_secrets, & &1.name)}
                secret_name={secret.name}
                hub?={true}
                stored={hub_label(@hub)}
                active={
                  secret.name == @prefill_secret_name and
                    Session.Data.secret_toggled?(secret, @secrets)
                }
                target={@myself}
              />
              <div
                :if={@secrets == %{} and @hub_secrets == []}
                class="w-full text-center text-gray-400 border rounded-lg p-8"
              >
                <.remix_icon icon="folder-lock-line" class="align-middle text-2xl" />
                <span class="mt-1 block text-sm text-gray-700">
                  Secrets not found. <br /> Add to see them here.
                </span>
              </div>
            </div>
          </div>
        </div>
        <.form
          :let={f}
          for={@changeset}
          phx-target={@myself}
          phx-change="validate"
          phx-submit="save"
          autocomplete="off"
          class="basis-1/2 grow"
        >
          <div class="flex flex-col space-y-4">
            <p :if={@select_secret_ref} class="text-gray-700">
              Add new secret
            </p>
            <.text_field
              field={f[:name]}
              label="Name (alphanumeric and underscore)"
              autofocus={@prefill_secret_name == nil}
              spellcheck="false"
              autocomplete="off"
              phx-debounce
              class="uppercase"
            />
            <.password_field
              field={f[:value]}
              label="Value"
              autofocus={@prefill_secret_name != nil}
              spellcheck="false"
              autocomplete="off"
              phx-debounce
            />
            <.radio_field
              field={f[:hub_id]}
              options={[
                {"", "only this session"},
                {@hub.id, "in #{@hub.hub_emoji} #{@hub.hub_name}"}
              ]}
            />
            <div class="flex space-x-2">
              <button class="button-base button-blue" type="submit" disabled={not @changeset.valid?}>
                <.remix_icon icon="add-line" class="align-middle" />
                <span class="font-normal">Add</span>
              </button>
              <.link patch={@return_to} class="button-base button-outlined-gray">
                Cancel
              </.link>
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
      phx-click="select_secret"
      phx-value-name={@secret_name}
      phx-value-hub={@hub?}
      phx-target={@target}
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
        <svg
          :if={@active}
          class="-ml-0.5 mr-1.5 h-2 w-2 text-blue-400"
          fill="currentColor"
          viewBox="0 0 8 8"
        >
          <circle cx="4" cy="4" r="3" />
        </svg>
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
                <span class="font-semibold text-white"><%= @secret.name %></span>
                in <%= hub_label(@hub) %>. Allow this session to access it?
              </span>
            </div>
            <button
              class="button-base button-gray"
              phx-click="select_secret"
              phx-value-name={@secret.name}
              phx-value-hub={true}
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
  def handle_event("save", %{"secret" => attrs}, socket) do
    with {:ok, secret} <- Secrets.update_secret(%Secret{}, attrs),
         :ok <- set_secret(socket, secret) do
      {:noreply,
       socket
       |> push_patch(to: socket.assigns.return_to)
       |> push_secret_selected(secret.name)}
    else
      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}

      {:transport_error, error} ->
        {:noreply,
         socket
         |> push_patch(to: ~p"/sessions/#{socket.assigns.session.id}/secrets")
         |> put_flash(:error, error)}
    end
  end

  def handle_event("validate", %{"secret" => attrs}, socket) do
    changeset =
      %Secret{}
      |> Secrets.change_secret(attrs)
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("select_secret", %{"name" => secret_name} = attrs, socket) do
    if attrs["hub"] do
      secret = Enum.find(socket.assigns.hub_secrets, &(&1.name == secret_name))

      unless Session.Data.secret_toggled?(secret, socket.assigns.secrets) do
        Session.set_secret(socket.assigns.session.pid, secret)
      end
    end

    {:noreply,
     socket
     |> push_patch(to: socket.assigns.return_to)
     |> push_secret_selected(secret_name)}
  end

  defp push_secret_selected(%{assigns: %{select_secret_ref: nil}} = socket, _), do: socket

  defp push_secret_selected(%{assigns: %{select_secret_ref: ref}} = socket, secret_name) do
    push_event(socket, "secret_selected", %{select_secret_ref: ref, secret_name: secret_name})
  end

  defp title(%{assigns: %{select_secret_ref: nil}}), do: "Add secret"
  defp title(%{assigns: %{select_secret_options: %{"title" => title}}}), do: title
  defp title(_), do: "Select secret"

  defp set_secret(socket, %Secret{hub_id: nil} = secret) do
    Session.set_secret(socket.assigns.session.pid, secret)
  end

  defp set_secret(socket, %Secret{} = secret) do
    with :ok <- Hubs.create_secret(socket.assigns.hub, secret) do
      Session.set_secret(socket.assigns.session.pid, secret)
    end
  end

  defp hub_label(hub), do: "#{hub.hub_emoji} #{hub.hub_name}"
end
