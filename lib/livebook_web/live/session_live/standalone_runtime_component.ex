defmodule LivebookWeb.SessionLive.StandaloneRuntimeComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  @impl true
  def mount(socket) do
    unless Livebook.Config.runtime_enabled?(Livebook.Runtime.Standalone) do
      raise "runtime module not allowed"
    end

    {:ok, socket}
  end

  @impl true
  def update(assigns, socket) do
    changeset =
      case socket.assigns[:changeset] do
        nil ->
          changeset(assigns.runtime)

        changeset when socket.assigns.runtime == assigns.runtime ->
          changeset

        changeset ->
          changeset(assigns.runtime, changeset.params)
      end

    socket =
      socket
      |> assign(assigns)
      |> assign(:changeset, changeset)

    {:ok, socket}
  end

  defp changeset(runtime, attrs \\ %{}) do
    data =
      case runtime do
        %Livebook.Runtime.Standalone{erl_flags: erl_flags} ->
          %{erl_flags: erl_flags}

        _ ->
          %{erl_flags: nil}
      end

    types = %{erl_flags: :string}

    cast({data, types}, attrs, [:erl_flags])
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={@id} class="flex-col space-y-5">
      <p class="text-gray-700">
        Start a new local Elixir node to evaluate code. Whenever you reconnect this runtime,
        a fresh node is started.
      </p>
      <.form
        :let={f}
        for={@changeset}
        as={:data}
        phx-submit="init"
        phx-change="validate"
        phx-target={@myself}
        autocomplete="off"
        spellcheck="false"
      >
        <div id={"#{@id}-advanced"} class="mb-6">
          <div
            class="flex items-center gap-0.5 text-gray-700 font-medium cursor-pointer"
            phx-click={JS.toggle(to: "##{@id}-advanced [data-toggle]")}
          >
            <span>Advanced configuration</span>
            <.remix_icon icon="arrow-down-s-line" class="text-xl hidden" data-toggle />
            <.remix_icon icon="arrow-right-s-line" class="text-xl" data-toggle />
          </div>
          <div class="mt-2 flex flex-col space-y-4 hidden" data-toggle>
            <.text_field field={f[:erl_flags]} label="Erl flags" />
          </div>
        </div>
        <.button type="submit" disabled={@runtime_status == :connecting or not @changeset.valid?}>
          {label(@changeset, @runtime_status)}
        </.button>
      </.form>
    </div>
    """
  end

  defp label(changeset, runtime_status) do
    reconnecting? = changeset.valid? and changeset.data == apply_changes(changeset)

    case {reconnecting?, runtime_status} do
      {true, :connected} -> "Reconnect"
      {true, :connecting} -> "Connecting..."
      _ -> "Connect"
    end
  end

  @impl true
  def handle_event("validate", %{"data" => data}, socket) do
    changeset =
      socket.assigns.runtime
      |> changeset(data)
      |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("init", %{"data" => data}, socket) do
    socket.assigns.runtime
    |> changeset(data)
    |> apply_action(:insert)
    |> case do
      {:ok, data} ->
        runtime = Livebook.Runtime.Standalone.new(erl_flags: data.erl_flags)
        Livebook.Session.set_runtime(socket.assigns.session.pid, runtime)
        Livebook.Session.connect_runtime(socket.assigns.session.pid)
        {:noreply, socket}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
end
