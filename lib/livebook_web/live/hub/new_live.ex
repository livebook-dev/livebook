defmodule LivebookWeb.Hub.NewLive do
  use LivebookWeb, :live_view

  alias Livebook.Hubs.Enterprise
  alias Livebook.Teams
  alias Livebook.Teams.Org
  alias LivebookWeb.LayoutHelpers
  alias Phoenix.LiveView.JS

  on_mount(LivebookWeb.SidebarHook)

  @impl true
  def mount(_params, _session, socket) do
    enabled? = Livebook.Config.feature_flag_enabled?(:create_hub)

    {:ok,
     assign(socket,
       selected_option: nil,
       page_title: "Hub - Livebook",
       enabled?: enabled?,
       requested_code: false,
       org: nil,
       verification_uri: nil,
       org_form: nil
     )}
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

        <.org_form
          form={@org_form}
          org={@org}
          requested_code={@requested_code}
          selected={@selected_option}
          verification_uri={@verification_uri}
        />
      </div>
    </LayoutHelpers.layout>
    """
  end

  defp org_form(assigns) do
    ~H"""
    <div class="flex flex-col space-y-4">
      <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
        1. Select your option
      </h2>

      <div class="grid grid-cols-1 sm:grid-cols-2 gap-4">
        <.card_item id="new-org" selected={@selected} title="Create a new organization">
          <:logo><.remix_icon icon="add-circle-fill" class="text-black text-3xl" /></:logo>
          <:headline>Create a new organization and invite your team members.</:headline>
        </.card_item>

        <.card_item id="join-org" selected={@selected} disabled title="Join an organization">
          <:logo><.remix_icon icon="user-add-fill" class="text-black text-3xl" /></:logo>
          <:headline>Coming soon...</:headline>
        </.card_item>
      </div>
    </div>

    <div :if={@selected == "new-org"} class="flex flex-col space-y-4">
      <h2 class="text-xl text-gray-800 font-medium pb-2 border-b border-gray-200">
        2. Create your Organization
      </h2>

      <.form
        :let={f}
        id="new-org-form"
        class="flex flex-col space-y-4"
        for={@form}
        phx-submit="save"
        phx-change="validate"
      >
        <div class="grid grid-cols-1 md:grid-cols-2 gap-3">
          <.text_field field={f[:name]} label="Name" />
          <.emoji_field field={f[:emoji]} label="Emoji" />
        </div>

        <.password_field readonly field={f[:teams_key]} label="Livebook Teams Key" />

        <div :if={@requested_code} class="grid grid-cols-1 gap-3">
          <span>
            Access the following URL and input the User Code below to confirm the Organization creation.
          </span>

          <.link navigate={@verification_uri} target="_blank" class="font-bold text-blue-500">
            <%= @verification_uri %>
          </.link>

          <span><%= @org.user_code %></span>
        </div>

        <button :if={!@requested_code} class="button-base button-blue" phx-disable-with="Creating...">
          Create Org
        </button>
      </.form>
    </div>
    """
  end

  defp card_item(assigns) do
    assigns = assign_new(assigns, :disabled, fn -> false end)

    ~H"""
    <div
      id={@id}
      class={["flex flex-col cursor-pointer", disabled_class(@disabled)]}
      phx-click={JS.push("select_option", value: %{value: @id})}
    >
      <div class={[
        "flex items-center justify-center p-6 border-2 rounded-t-2xl h-[150px]",
        card_item_border_class(@id, @selected)
      ]}>
        <%= render_slot(@logo) %>
      </div>
      <div class={["px-6 py-4 rounded-b-2xl grow", card_item_class(@id, @selected)]}>
        <p class="text-gray-800 font-semibold cursor-pointer">
          <%= @title %>
        </p>

        <p class="mt-2 text-sm text-gray-600">
          <%= render_slot(@headline) %>
        </p>
      </div>
    </div>
    """
  end

  defp disabled_class(true), do: "opacity-30 pointer-events-none"
  defp disabled_class(false), do: ""

  defp card_item_border_class(id, id), do: "border-gray-200"
  defp card_item_border_class(_, _), do: "border-gray-100"

  defp card_item_class(id, id), do: "bg-gray-200"
  defp card_item_class(_, _), do: "bg-gray-100"

  @impl true
  def handle_event("select_option", %{"value" => option}, socket) do
    {:noreply,
     socket
     |> assign(selected_option: option)
     |> assign_form(option)}
  end

  def handle_event("validate", %{"new_org" => attrs}, socket) do
    changeset =
      socket.assigns.org
      |> Teams.change_org(attrs)
      |> Map.replace!(:action, :validate)

    {:noreply, assign_form(socket, changeset)}
  end

  def handle_event("save", %{"new_org" => attrs}, socket) do
    case Teams.create_org(socket.assigns.org, attrs) do
      {:ok, response} ->
        attrs = Map.merge(attrs, response)
        changeset = Teams.change_org(socket.assigns.org, attrs)
        org = Ecto.Changeset.apply_action!(changeset, :insert)

        Process.send_after(self(), {:check_completion_data, org}, 1000)

        {:noreply,
         socket
         |> assign(
           requested_code: true,
           org: org,
           verification_uri: response["verification_uri"]
         )
         |> assign_form(changeset)}

      {:error, changeset} ->
        {:noreply, assign_form(socket, changeset)}

      {:transport_error, message} ->
        {:noreply, put_flash(socket, :error, message)}
    end
  end

  @impl true
  def handle_info({:check_completion_data, %Org{} = org}, socket) do
    case Teams.get_org_request_completion_data(org) do
      {:ok, :awaiting_confirmation} ->
        Process.send_after(self(), {:check_completion_data, org}, 1000)

        {:noreply, socket}

      {:ok, response} ->
        attrs = %{
          org_id: response["id"],
          user_id: response["user_id"],
          org_key_id: response["org_key_id"],
          session_token: response["session_token"],
          teams_key: org.teams_key,
          hub_name: response["name"],
          hub_emoji: org.emoji
        }

        case Enterprise.create_hub(%Enterprise{}, attrs) do
          {:ok, hub} ->
            {:noreply,
             socket
             |> put_flash(:success, "Hub added successfully")
             |> push_navigate(to: ~p"/hub/#{hub.id}")}

          {:error, changeset} ->
            {:noreply, assign_form(socket, changeset)}
        end

      {:error, :expired} ->
        org = Map.replace!(socket.assigns.org, :user_code, "requested")
        changeset = Teams.change_org(org, %{})

        {:noreply,
         socket
         |> assign(
           requested_code: false,
           org: org,
           verification_uri: nil,
           created: false
         )
         |> put_flash(
           :error,
           "Oh no! Your org creation request expired, could you please try again?"
         )
         |> assign_form(changeset)}

      {:transport_error, message} ->
        {:noreply, put_flash(socket, :error, message)}
    end
  end

  def handle_info(_any, socket), do: {:noreply, socket}

  defp assign_form(socket, "new-org") do
    org = %Org{user_code: "request", emoji: "🏭️"}
    changeset = Teams.change_org(org)

    socket
    |> assign(org: org)
    |> assign_form(changeset)
  end

  defp assign_form(socket, %Ecto.Changeset{} = changeset) do
    assign(socket, org_form: to_form(changeset, as: :new_org))
  end
end
