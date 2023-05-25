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
    enabled? = Livebook.Config.feature_flag_enabled?(:create_hub)

    socket =
      assign(socket,
        selected_option: "new-org",
        page_title: "Hub - Livebook",
        enabled?: enabled?,
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
  def render(%{enabled?: false} = assigns) do
    ~H"""
    <LayoutHelpers.layout current_page="/hub" current_user={@current_user} saved_hubs={@saved_hubs}>
      <div class="p-4 md:px-12 md:py-7 max-w-screen-md mx-auto space-y-6">
        <div>
          <LayoutHelpers.title text="Hubs are coming soon!" />
          <p class="mt-4 text-gray-700">
            Deploy applications, share secrets, templates, and more with Livebook Hubs.
          </p>
        </div>
        <p class="text-gray-700">
          Each Livebook user has their own personal Hub and soon they will be able to deploy
          their personal notebooks to
          <a
            class="font-medium underline text-gray-900 hover:no-underline"
            href="https://fly.io/"
            target="_blank"
          >
            Fly.io
          </a>
          and <a
            class="font-medium underline text-gray-900 hover:no-underline"
            href="https://huggingface.co/"
            target="_blank"
          >Hugging Face</a>.
        </p>
        <p class="text-gray-700">
          We are also working on <span class="font-bold">Livebook Teams</span>, which were
          designed from the ground up to deploy notebooks within your organization.
          <span class="font-bold">Livebook Teams</span>
          runs on your own infrastructure
          to provide essential features for secure collaboration between team members,
          such as digital signing of notebooks, safe sharing of secrets, and more.
          To learn more, <a
            class="font-medium underline text-gray-900 hover:no-underline"
            href="https://docs.google.com/forms/d/e/1FAIpQLScDfvUqT4f_s95dqNGyoXwVMD_Vl059jT6r5MPgXB99XVMCuw/viewform?usp=sf_link"
            target="_blank"
          >get in touch</a>!
        </p>
        <p class="text-gray-700">
          - The Livebook crew
        </p>
      </div>
    </LayoutHelpers.layout>
    """
  end

  def render(assigns) do
    ~H"""
    <LayoutHelpers.layout current_page="/hub" current_user={@current_user} saved_hubs={@saved_hubs}>
      <div class="p-4 md:px-12 md:py-7 max-w-screen-md mx-auto space-y-8">
        <div>
          <LayoutHelpers.title text="Add Hub" />
          <p class="mt-4 text-gray-700">
            Manage your Livebooks in the cloud with Hubs.
          </p>
        </div>
        <!-- TABS -->
        <div class="flex flex-col space-y-4">
          <div class="relative flex flex-col items-stretch justify-center gap-2 sm:items-center">
            <div class="relative flex rounded-xl bg-gray-100 p-1">
              <ul class="flex w-full list-none gap-1 sm:w-auto">
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
              <.text_field field={f[:name]} label="Name" />
              <.emoji_field field={f[:emoji]} label="Emoji" />
            </div>

            <.password_field
              :if={@selected_option == "join-org"}
              field={f[:teams_key]}
              label="Livebook Teams Key"
            />

            <div :if={@requested_code} class="grid grid-cols-1 gap-3">
              <span><%= @request_code_info %></span>

              <.link navigate={@verification_uri} target="_blank" class="font-bold text-blue-500">
                <%= @verification_uri %>
              </.link>

              <span><%= @org.user_code %></span>
            </div>

            <button
              :if={!@requested_code}
              class="button-base button-blue self-start"
              phx-disable-with="Creating..."
            >
              <%= @button_label %>
            </button>
          </.form>
        </div>
      </div>
    </LayoutHelpers.layout>
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
          "group button relative flex w-full items-center justify-center gap-1 rounded-lg border py-3 transition-opacity duration-100 sm:w-auto sm:min-w-[250px] md:gap-2 md:py-2.5",
          selected_tab_button(@id, @selected)
        ]}>
          <span class="relative max-[370px]:hidden">
            <.remix_icon
              icon={@icon}
              class={[
                "group-hover:text-blue-500 text-lg",
                if @selected == @id do
                  "text-blue-500"
                else
                  "text-gray-500"
                end
              ]}
            />
          </span>
          <span class="truncate text-sm font-medium"><%= @title %></span>
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
        changeset = Teams.change_org(org, %{user_code: nil})

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
    org = %Org{emoji: "💡"}
    changeset = Teams.change_org(org)

    socket
    |> assign(
      org: org,
      button_label: "Join",
      request_code_info:
        "Access the following URL and input the User Code below to confirm the Organization creation."
    )
    |> assign_form(changeset)
  end

  defp assign_form(socket, "new-org") do
    org = %Org{emoji: "⭐️", teams_key: Org.teams_key()}
    changeset = Teams.change_org(org)

    socket
    |> assign(
      org: org,
      button_label: "Create",
      request_code_info:
        "Access the following URL and input the User Code below to confirm to join an Organization."
    )
    |> assign_form(changeset)
  end

  defp assign_form(socket, %Ecto.Changeset{} = changeset) do
    assign(socket, form: to_form(changeset))
  end
end
