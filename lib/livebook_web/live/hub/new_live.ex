defmodule LivebookWeb.Hub.NewLive do
  use LivebookWeb, :live_view

  alias Livebook.Teams
  alias Livebook.Teams.Org
  alias LivebookWeb.LayoutHelpers

  on_mount LivebookWeb.SidebarHook

  @check_completion_data_interval Application.compile_env(
                                    :livebook,
                                    :check_completion_data_interval,
                                    3000
                                  )

  @impl true
  def mount(_params, _session, socket) do
    socket =
      assign(socket,
        selected_option: "new-org",
        page_title: "Hub - Livebook",
        requested_code: false,
        org: nil,
        verification_uri: nil,
        form: nil,
        button_label: nil,
        request_code_info: nil
      )

    socket = assign_form(socket, "new-org")

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <LayoutHelpers.layout current_page="/hub" current_user={@current_user} saved_hubs={@saved_hubs}>
      <LayoutHelpers.topbar :if={Livebook.Config.warn_on_live_teams_server?()} variant={:warning}>
        <strong>Beware!</strong>
        You are running Livebook in development but this page communicates with production servers.
      </LayoutHelpers.topbar>

      <div class="flex flex-col p-4 md:px-12 md:py-7 max-w-screen-md mx-auto space-y-8">
        <div>
          <LayoutHelpers.title text="Add organization" />
          <p class="mt-4 text-gray-700">
            <a
              class="font-medium underline text-gray-900 hover:no-underline"
              href="https://livebook.dev/teams?ref=LivebookApp"
              target="_blank"
              phx-no-format
            >
              Livebook Teams</a> amplifies Livebook with features designed for teams and businesses. It is currently in closed beta.
          </p>
          <p class="mt-4 text-gray-700">
            To create a Teams organization, you must <a
              class="font-medium underline text-gray-900 hover:no-underline"
              href="https://livebook.dev/teams?ref=LivebookApp"
              target="_blank"
            >join the beta for free early access</a>.
          </p>
        </div>
        <!-- TABS -->
        <div class="flex flex-col space-y-4">
          <div class="flex flex-col justify-center sm:items-center sm:m-auto">
            <div class="flex rounded-xl bg-gray-100 p-1">
              <ul class="flex flex-col sm:flex-row md:flex-col lg:flex-row w-full list-none gap-1">
                <!-- New Org -->
                <.tab_button
                  id="new-org"
                  selected={@selected_option}
                  title="Create a new organization"
                  icon="lightbulb-flash-line"
                />
                <!-- Join Org -->
                <.tab_button
                  id="join-org"
                  selected={@selected_option}
                  title="Join an existing organization"
                  icon="organization-chart"
                />
              </ul>
            </div>
          </div>
        </div>
        <!-- FORMS -->
        <div :if={@selected_option} class="flex flex-col space-y-4">
          <.form
            :let={f}
            id={"#{@selected_option}-form"}
            class="flex flex-col space-y-4"
            for={@form}
            phx-submit="save"
            phx-change="validate"
          >
            <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
              <.text_field field={f[:name]} label="Organization name" />
              <.emoji_field field={f[:emoji]} label="Emoji" />
            </div>

            <.password_field
              :if={@selected_option == "join-org"}
              field={f[:teams_key]}
              label="Livebook Teams key"
            />

            <button
              :if={!@requested_code}
              class="button-base button-blue self-start"
              phx-disable-with="Loading..."
            >
              <%= @button_label %>
            </button>
            <div class="invisible"></div>
            <div :if={@requested_code} class="flex flex-col rounded-xl bg-gray-50 px-10 py-6 mt-10">
              <div class="flex flex-col items-center rounded-xl bg-gray-50">
                <span class="text-base font-semibold text-center text-gray-900">
                  <%= @request_code_info %>
                </span>
                <div class="text-center mt-4 text-gray-700">
                  <span class="text-sm">
                    1. Copy the code:
                  </span>
                  <div class="mt-3 text-center">
                    <.copyclip content={@org.user_code} />
                  </div>
                </div>
                <div class="text-center mt-4 text-gray-700">
                  <span class="text-sm">
                    2. Sign in to Livebook Teams and paste the code:
                  </span>
                  <div>
                    <a
                      href={@verification_uri}
                      target="_blank"
                      class="mt-3 button-base button-outlined-gray"
                    >
                      Go to Teams
                    </a>
                  </div>
                </div>
              </div>
            </div>
          </.form>
        </div>
      </div>
    </LayoutHelpers.layout>
    """
  end

  defp copyclip(assigns) do
    ~H"""
    <div
      id="clipboard"
      class="flex items-center justify-between border rounded-lg px-4 py-2.5 bg-white"
    >
      <div class="icon-button invisible">
        <.remix_icon icon="clipboard-line" class="text-lg" />
      </div>

      <div
        class="text-brand-pink font-semibold text-xl leading-none"
        id="clipboard-code"
        phx-no-format
      ><%= @content %></div>

      <button
        class="icon-button ml-4"
        data-el-clipcopy
        phx-click={JS.dispatch("lb:clipcopy", to: "#clipboard-code")}
        type="button"
      >
        <.remix_icon icon="clipboard-line" class="text-lg text-blue-500" />
      </button>
    </div>
    """
  end

  defp tab_button(assigns) do
    ~H"""
    <li class="group/toggle w-full">
      <button
        type="button"
        id={@id}
        aria-haspopup="menu"
        aria-expanded="false"
        data-state="closed"
        class="w-full"
        phx-click="select_option"
        phx-value-option={@id}
      >
        <div class={[
          "group button flex w-full sm:w-72 items-center justify-center gap-1 md:gap-2 rounded-lg border py-3 md:py-2.5 px-5 transition-opacity duration-100",
          selected_tab_button(@id, @selected)
        ]}>
          <.remix_icon
            icon={@icon}
            class={[
              "group-hover:text-blue-600 text-lg",
              if @selected == @id do
                "text-blue-600"
              else
                "text-gray-500"
              end
            ]}
          />
          <span class="truncate text-sm font-medium">
            <%= @title %>
          </span>
        </div>
      </button>
    </li>
    """
  end

  defp selected_tab_button(id, id),
    do: "border-black/10 bg-white drop-shadow-sm hover:!opacity-100"

  defp selected_tab_button(_, _), do: "border-transparent text-gray-500 hover:text-gray-800"

  @impl true
  def handle_event("select_option", %{"option" => option}, socket) do
    {:noreply,
     socket
     |> assign(selected_option: option, requested_code: false, verification_uri: nil)
     |> assign_form(option)}
  end

  def handle_event("validate", %{"org" => attrs}, socket) do
    changeset =
      socket.assigns.org
      |> Teams.change_org(attrs)
      |> Map.replace!(:action, :validate)

    {:noreply, assign_form(socket, changeset)}
  end

  def handle_event("save", %{"org" => attrs}, socket) do
    result =
      case socket.assigns.selected_option do
        "new-org" -> Teams.create_org(socket.assigns.org, attrs)
        "join-org" -> Teams.join_org(socket.assigns.org, attrs)
      end

    case result do
      {:ok, %{"device_code" => device_code} = response} ->
        attrs = Map.merge(attrs, response)
        changeset = Teams.change_org(socket.assigns.org, attrs)
        org = Ecto.Changeset.apply_action!(changeset, :insert)

        Process.send_after(
          self(),
          {:check_completion_data, device_code},
          @check_completion_data_interval
        )

        {:noreply,
         socket
         |> assign(requested_code: true, org: org, verification_uri: response["verification_uri"])
         |> assign_form(changeset)}

      {:error, changeset} ->
        changeset = Map.replace!(changeset, :action, :validate)

        {:noreply, assign_form(socket, changeset)}

      {:transport_error, message} ->
        {:noreply, put_flash(socket, :error, message)}
    end
  end

  @impl true
  def handle_info({:check_completion_data, device_code}, %{assigns: %{org: org}} = socket) do
    case Teams.get_org_request_completion_data(org, device_code) do
      {:ok, :awaiting_confirmation} ->
        Process.send_after(
          self(),
          {:check_completion_data, device_code},
          @check_completion_data_interval
        )

        {:noreply, socket}

      {:ok, %{"id" => _id, "session_token" => _session_token} = response} ->
        hub =
          Teams.create_hub!(%{
            org_id: response["id"],
            user_id: response["user_id"],
            org_key_id: response["org_key_id"],
            org_public_key: response["org_public_key"],
            session_token: response["session_token"],
            teams_key: org.teams_key,
            hub_name: org.name,
            hub_emoji: org.emoji
          })

        {:noreply,
         socket
         |> put_flash(:success, "Hub added successfully")
         |> push_navigate(to: ~p"/hub/#{hub.id}?show-key=true")}

      {:error, :expired} ->
        changeset =
          org
          |> Teams.change_org(%{user_code: nil})
          |> Map.replace!(:action, :validate)

        {:noreply,
         socket
         |> assign(requested_code: false, org: org, verification_uri: nil)
         |> put_flash(:error, "Oh no! Your org request expired, could you please try again?")
         |> assign_form(changeset)}

      {:transport_error, message} ->
        Process.send_after(
          self(),
          {:check_completion_data, device_code},
          @check_completion_data_interval
        )

        {:noreply, put_flash(socket, :error, message)}
    end
  end

  def handle_info(_any, socket), do: {:noreply, socket}

  defp assign_form(socket, "join-org") do
    org = %Org{emoji: random_emoji()}
    changeset = Teams.change_org(org)

    socket
    |> assign(
      org: org,
      button_label: "Join",
      request_code_info: "Authenticate with your organization"
    )
    |> assign_form(changeset)
  end

  defp assign_form(socket, "new-org") do
    org = %Org{emoji: random_emoji(), teams_key: Org.teams_key()}
    changeset = Teams.change_org(org)

    socket
    |> assign(
      org: org,
      button_label: "Create",
      request_code_info: "Verify your new organization"
    )
    |> assign_form(changeset)
  end

  defp assign_form(socket, %Ecto.Changeset{} = changeset) do
    assign(socket, form: to_form(changeset))
  end

  defp random_emoji do
    Enum.random(~w[💡 🚀 🌈 🦄 🐱 👩‍💻 ⚽️ ⭐️])
  end
end
