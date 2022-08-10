defmodule LivebookWeb.HubLive do
  use LivebookWeb, :live_view

  import LivebookWeb.UserHelpers

  alias Livebook.Hub
  alias Livebook.Hub.Fly
  alias Livebook.Hub.Settings
  alias Livebook.Users.User
  alias LivebookWeb.{PageHelpers, SidebarHelpers}
  alias Phoenix.LiveView.JS

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> SidebarHelpers.sidebar_handlers()
     |> assign(
       selected_hub_service: nil,
       hubs: [],
       hub_options: [],
       data: %{},
       page_title: "Livebook - Hub"
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex grow h-full">
      <SidebarHelpers.sidebar
        socket={@socket}
        current_user={@current_user}
        current_page=""
        saved_hubs={@saved_hubs}
      />

      <div class="grow px-6 py-8 overflow-y-auto">
        <div class="max-w-screen-md w-full mx-auto px-4 pb-8 space-y-8">
          <div>
            <PageHelpers.title text="Hub" socket={@socket} />
            <p class="mt-4 text-gray-700">
              Manage your Livebooks in the cloud with Hubs.
            </p>
          </div>

          <div class="flex flex-col space-y-4">
            <h2 class="text-xl text-gray-800 font-semibold pb-2 border-b border-gray-200">
              1. Select your Hub service
            </h2>

            <div class="grid grid-cols-1 md:grid-cols-2 gap-4">
              <.card_item id="fly" selected={@selected_hub_service} title="Fly">
                <:logo>
                  <%= Phoenix.HTML.raw(File.read!("static/images/fly.io.svg")) %>
                </:logo>
                <:headline>
                  Deploy notebooks to your Fly account.
                </:headline>
              </.card_item>

              <.card_item id="enterprise" selected={@selected_hub_service} title="Livebook Enterprise">
                <:logo>
                  <img src="/images/logo.png" class="max-h-full max-w-[75%]" alt="Fly logo" />
                </:logo>
                <:headline>
                  Control access, manage secrets, and deploy notebooks within your team and company.
                </:headline>
              </.card_item>
            </div>
          </div>

          <%= if @selected_hub_service do %>
            <div class="flex flex-col space-y-4">
              <h2 class="text-xl text-gray-800 font-semibold pb-2 border-b border-gray-200">
                2. Configure your Hub
              </h2>

              <%= if @selected_hub_service == "fly" do %>
                <.fly_form socket={@socket} data={@data} hubs={@hub_options} />
              <% end %>

              <%= if @selected_hub_service == "enterprise" do %>
                <div>
                  Livebook Enterprise is currently in closed beta. If you want to learn more, <a
                    href="https://livebook.dev/#livebook-plans"
                    class="pointer-events-auto text-blue-600"
                    target="_blank"
                  >click here</a>.
                </div>
              <% end %>
            </div>
          <% end %>
        </div>
      </div>

      <.current_user_modal current_user={@current_user} />
    </div>
    """
  end

  defp card_item(assigns) do
    ~H"""
    <div
      id={@id}
      class={"flex card-item flex-col " <> card_item_bg_color(@id, @selected)}
      phx-click={JS.push("select_hub_service", value: %{value: @id})}
    >
      <div class="flex items-center justify-center card-item--logo p-6 border-2 rounded-t-2xl h-[150px]">
        <%= render_slot(@logo) %>
      </div>
      <div class="card-item--body px-6 py-4 rounded-b-2xl grow">
        <p class="text-gray-800 font-semibold cursor-pointer mt-2 text-sm text-gray-600">
          <%= @title %>
        </p>

        <p class="mt-2 text-sm text-gray-600">
          <%= render_slot(@headline) %>
        </p>
      </div>
    </div>
    """
  end

  defp card_item_bg_color(id, selected) when id == selected, do: "selected"
  defp card_item_bg_color(_id, _selected), do: ""

  defp fly_form(assigns) do
    ~H"""
    <.form
      id="fly-form"
      class="flex flex-col space-y-4"
      let={f}
      for={:fly}
      phx-submit="save_hub"
      phx-change="update_data"
      phx-debounce="blur"
    >
      <div class="flex flex-col space-y-1">
        <h3 class="text-gray-800 font-semibold">
          Access Token
        </h3>
        <%= password_input(f, :token,
          phx_change: "fetch_hubs",
          phx_debounce: "blur",
          value: @data["token"],
          class: "input w-full",
          autofocus: true,
          spellcheck: "false",
          required: true,
          autocomplete: "off"
        ) %>
      </div>

      <%= if length(@hubs) > 0 do %>
        <div class="flex flex-col space-y-1">
          <h3 class="text-gray-800 font-semibold">
            Organization
          </h3>
          <%= select(f, :organization, @hubs, class: "input", required: true) %>
        </div>

        <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
          <div class="flex flex-col space-y-1">
            <h3 class="text-gray-800 font-semibold">
              Name
            </h3>
            <%= text_input(f, :name, value: @data["name"], class: "input", required: true) %>
          </div>

          <div class="flex flex-col space-y-1">
            <h3 class="text-gray-800 font-semibold">
              Color
            </h3>

            <div class="flex space-x-4 items-center">
              <div
                class="border-[3px] rounded-lg p-1 flex justify-center items-center"
                style={"border-color: #{@data["hex_color"]}"}
              >
                <div class="rounded h-5 w-5" style={"background-color: #{@data["hex_color"]}"}></div>
              </div>
              <div class="relative grow">
                <%= text_input(f, :hex_color,
                  value: @data["hex_color"],
                  class: "input",
                  spellcheck: "false",
                  required: true,
                  maxlength: 7
                ) %>
                <button
                  class="icon-button absolute right-2 top-1"
                  type="button"
                  phx-click="randomize_color"
                >
                  <.remix_icon icon="refresh-line" class="text-xl" />
                </button>
              </div>
            </div>
          </div>
        </div>

        <%= submit("Save", class: "button-base button-blue", phx_disable_with: "Saving...") %>
      <% end %>
    </.form>
    """
  end

  @impl true
  def handle_params(%{"id" => id}, _url, socket) do
    hub = Settings.hub_by_id!(id)
    hubs = [hub]

    data =
      case hub.type do
        "fly" ->
          %{
            "organization" => hub.id,
            "token" => hub.token,
            "name" => hub.name,
            "hex_color" => hub.color
          }
      end

    opts = select_hub_options(hubs, data["organization"])

    {:noreply,
     assign(socket,
       operation: :edit,
       selected_hub_service: hub.type,
       data: data,
       hubs: hubs,
       hub_options: opts
     )}
  end

  def handle_params(_params, _url, socket) do
    {:noreply, assign(socket, operation: :new)}
  end

  @impl true
  def handle_event("select_hub_service", %{"value" => service}, socket) do
    {:noreply, assign(socket, selected_hub_service: service)}
  end

  def handle_event("fetch_hubs", %{"fly" => %{"token" => token}}, socket) do
    case Hub.fetch_hubs(%Fly{token: token}) do
      {:ok, hubs} ->
        data = %{"token" => token, "hex_color" => User.random_hex_color()}
        opts = select_hub_options(hubs)

        {:noreply, assign(socket, data: data, hubs: hubs, hub_options: opts)}

      {:error, _} ->
        {:noreply,
         socket
         |> assign(hubs: [], hub_options: [], data: %{})
         |> put_flash(:error, "Invalid Access Token")}
    end
  end

  def handle_event("save_hub", %{"fly" => params}, socket) do
    case Enum.find(socket.assigns.hubs, &(&1.id == params["organization"])) do
      nil ->
        {:noreply,
         socket
         |> assign(data: params)
         |> put_flash(:error, "Internal Server Error")}

      selected_hub ->
        case {socket.assigns.operation, Settings.hub_exists?(selected_hub)} do
          {:new, false} ->
            {:noreply, save_fly_hub(socket, params, selected_hub)}

          {:edit, true} ->
            {:noreply, save_fly_hub(socket, params, selected_hub)}

          _any ->
            {:noreply,
             socket
             |> assign(data: params)
             |> put_flash(:error, "Hub already exists")}
        end
    end
  end

  def handle_event("update_data", %{"fly" => data}, socket) do
    opts = select_hub_options(socket.assigns.hubs, data["organization"])

    {:noreply, assign(socket, data: data, hub_options: opts)}
  end

  def handle_event("randomize_color", _, socket) do
    data = Map.put(socket.assigns.data, "hex_color", User.random_hex_color())
    {:noreply, assign(socket, data: data)}
  end

  defp save_fly_hub(socket, params, selected_hub) do
    user = socket.assigns.current_user
    opts = select_hub_options(socket.assigns.hubs, params["organization"])

    Settings.save_hub(%{
      selected_hub
      | name: params["name"],
        color: params["hex_color"],
        token: params["token"]
    })

    Phoenix.PubSub.broadcast(Livebook.PubSub, "users:#{user.id}", :update_hub)

    assign(socket, data: params, hub_options: opts)
  end

  defp select_hub_options(hubs, hub_id \\ nil) do
    for hub <- hubs do
      if hub.id == hub_id do
        [key: hub.label, value: hub.id, selected: true]
      else
        [key: hub.label, value: hub.id]
      end
    end
  end
end
