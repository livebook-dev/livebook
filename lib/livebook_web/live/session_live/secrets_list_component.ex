defmodule LivebookWeb.SessionLive.SecretsListComponent do
  use LivebookWeb, :live_component

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
          <div
            :for={{secret_name, secret_value} <- Enum.sort(@secrets)}
            class="flex flex-col text-gray-500 rounded-lg px-2 pt-1"
            id={"session-secret-#{secret_name}-wrapper"}
          >
            <span
              class="text-sm font-mono break-all flex-row cursor-pointer"
              phx-click={
                JS.toggle(to: "#session-secret-#{secret_name}-detail", display: "flex")
                |> toggle_class("bg-gray-100", to: "#session-secret-#{secret_name}-wrapper")
              }
            >
              <%= secret_name %>
            </span>
            <div
              class="flex flex-row justify-between items-center my-1 hidden"
              id={"session-secret-#{secret_name}-detail"}
            >
              <span class="text-sm font-mono break-all flex-row">
                <%= secret_value %>
              </span>
              <button
                id={"session-secret-#{secret_name}-delete"}
                type="button"
                phx-click={
                  with_confirm(
                    JS.push("delete_session_secret",
                      value: %{secret_name: secret_name},
                      target: @myself
                    ),
                    title: "Delete session secret - #{secret_name}",
                    description: "Are you sure you want to delete this session secret?",
                    confirm_text: "Delete",
                    confirm_icon: "delete-bin-6-line"
                  )
                }
                class="hover:text-gray-900"
              >
                <.remix_icon icon="delete-bin-line" />
              </button>
            </div>
          </div>
        </div>

        <.link
          patch={~p"/sessions/#{@session.id}/secrets"}
          class="inline-flex items-center justify-center p-8 py-1 mt-6 space-x-2 text-sm font-medium text-gray-500 border border-gray-400 border-dashed rounded-xl hover:bg-gray-100"
          role="button"
        >
          <.remix_icon icon="add-line" class="text-lg align-center" />
          <span>New secret</span>
        </.link>

        <div class="mt-16">
          <h3 class="uppercase text-sm font-semibold text-gray-500">
            App secrets
          </h3>
          <span class="text-sm text-gray-500">
            <%= if @saved_secrets == [] do %>
              No secrets stored in Livebook so far
            <% else %>
              Toggle to share with this session
            <% end %>
          </span>
        </div>

        <div class="flex flex-col space-y-4 mt-6">
          <.secrets_item
            :for={secret when secret.origin in [:app, :startup] <- @saved_secrets}
            secret={secret}
            prefix={to_string(secret.origin)}
            data_secrets={@secrets}
            hubs={@hubs}
            myself={@myself}
          />
        </div>

        <div :if={Livebook.Config.feature_flag_enabled?(:hub)} class="mt-16">
          <h3 class="uppercase text-sm font-semibold text-gray-500">
            Hub secrets
          </h3>
          <span class="text-sm text-gray-500">
            <%= if @saved_secrets == [] do %>
              No secrets stored in Livebook so far
            <% else %>
              Toggle to share with this session
            <% end %>
          </span>
        </div>

        <div class="flex flex-col space-y-4 mt-6">
          <.secrets_item
            :for={%{origin: {:hub, id}} = secret <- @saved_secrets}
            secret={secret}
            prefix={"hub-#{id}"}
            data_secrets={@secrets}
            hubs={@hubs}
            myself={@myself}
          />
        </div>
      </div>
    </div>
    """
  end

  defp secrets_item(assigns) do
    ~H"""
    <div
      class="flex flex-col text-gray-500 rounded-lg px-2 pt-1"
      id={"#{@prefix}-secret-#{@secret.name}-wrapper"}
    >
      <div class="flex flex-col text-gray-800">
        <div class="flex flex-col">
          <div class="flex justify-between items-center">
            <span
              class="text-sm font-mono w-full break-all flex-row cursor-pointer"
              phx-click={
                JS.toggle(to: "##{@prefix}-secret-#{@secret.name}-detail", display: "flex")
                |> toggle_class("bg-gray-100", to: "##{@prefix}-secret-#{@secret.name}-wrapper")
              }
            >
              <%= @secret.name %>
            </span>
            <.form
              :let={f}
              id={"#{@prefix}-secret-#{@secret.name}-toggle"}
              for={%{"toggled" => secret_toggled?(@secret, @data_secrets)}}
              as={:data}
              phx-change="toggle_secret"
              phx-target={@myself}
            >
              <.switch_field
                field={f[:toggled]}
                label={secret_label(@secret, @hubs)}
                tooltip={secret_tooltip(@secret, @hubs)}
              />
              <.hidden_field field={f[:name]} value={@secret.name} />
              <.hidden_field field={f[:value]} value={@secret.value} />
            </.form>
          </div>
          <div
            class="flex flex-row justify-between items-center my-1 hidden"
            id={"#{@prefix}-secret-#{@secret.name}-detail"}
          >
            <span class="text-sm font-mono break-all flex-row">
              <%= @secret.value %>
            </span>
            <button
              :if={@secret.origin == :app}
              id={"#{@prefix}-secret-#{@secret.name}-delete"}
              type="button"
              phx-click={
                with_confirm(
                  JS.push("delete_app_secret", value: %{secret_name: @secret.name}, target: @myself),
                  title: "Delete app secret - #{@secret.name}",
                  description: "Are you sure you want to delete this app secret?",
                  confirm_text: "Delete",
                  confirm_icon: "delete-bin-6-line"
                )
              }
              class="hover:text-gray-900"
            >
              <.remix_icon icon="delete-bin-line" />
            </button>
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
        accessed by Smart cells and can be read as
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
      secret = %{name: data["name"], value: data["value"]}
      Livebook.Session.set_secret(socket.assigns.session.pid, secret)
    else
      Livebook.Session.unset_secret(socket.assigns.session.pid, data["name"])
    end

    {:noreply, socket}
  end

  def handle_event("delete_session_secret", %{"secret_name" => secret_name}, socket) do
    Livebook.Session.unset_secret(socket.assigns.session.pid, secret_name)
    {:noreply, socket}
  end

  def handle_event("delete_app_secret", %{"secret_name" => secret_name}, socket) do
    Livebook.Secrets.unset_secret(secret_name)
    {:noreply, socket}
  end

  defp secret_toggled?(secret, secrets) do
    Map.has_key?(secrets, secret.name) and secrets[secret.name] == secret.value
  end

  defp secret_label(%{origin: {:hub, id}}, hubs), do: fetch_hub!(id, hubs).emoji
  defp secret_label(_, _), do: nil

  defp secret_tooltip(%{origin: {:hub, id}}, hubs), do: fetch_hub!(id, hubs).name
  defp secret_tooltip(_, _), do: nil

  defp fetch_hub!(id, hubs) do
    Enum.find(hubs, &(&1.id == id)) || raise "unknown hub id: #{id}"
  end
end
