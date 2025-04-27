defmodule LivebookWeb.SessionLive.AttachedRuntimeComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  @impl true
  def mount(socket) do
    unless Livebook.Config.runtime_enabled?(Livebook.Runtime.Attached) do
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
        %Livebook.Runtime.Attached{node: node, cookie: cookie} ->
          %{name: Atom.to_string(node), cookie: Atom.to_string(cookie)}

        _ ->
          %{name: nil, cookie: nil}
      end

    types = %{name: :string, cookie: :string}

    cast({data, types}, attrs, [:name, :cookie])
    |> validate_required([:name, :cookie])
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <p class="text-gray-700">
        Connect the session to an already running node
        and evaluate code in the context of that node.
        The node must run Elixir {Livebook.Runtime.Attached.elixir_version_requirement()}.
        Make sure to give the node a name and a cookie, for example:
      </p>
      <div class="text-gray-700 markdown">
        <pre><code>iex --name {test_node()} --cookie mycookie -S mix</code></pre>
      </div>
      <p class="text-gray-700">
        Then enter the connection information below:
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
        <div class="flex flex-col space-y-4 mb-6">
          <.text_field field={f[:name]} label="Name" placeholder={test_node()} />
          <.text_field field={f[:cookie]} label="Cookie" placeholder="mycookie" />
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
        node = String.to_atom(data.name)
        cookie = String.to_atom(data.cookie)
        runtime = Livebook.Runtime.Attached.new(node, cookie)
        Livebook.Session.set_runtime(socket.assigns.session.pid, runtime)
        Livebook.Session.connect_runtime(socket.assigns.session.pid)
        {:noreply, socket}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  defp test_node() do
    "test@#{Livebook.Utils.node_host()}"
  end
end
