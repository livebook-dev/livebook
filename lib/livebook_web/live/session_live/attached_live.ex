defmodule LivebookWeb.SessionLive.AttachedLive do
  use LivebookWeb, :live_view

  import Ecto.Changeset

  alias Livebook.{Session, Runtime}

  @impl true
  def mount(
        _params,
        %{"session_pid" => session_pid, "current_runtime" => current_runtime},
        socket
      ) do
    session = Session.get_by_pid(session_pid)

    unless Livebook.Config.runtime_enabled?(Livebook.Runtime.Attached) do
      raise "runtime module not allowed"
    end

    if connected?(socket) do
      Session.subscribe(session.id)
    end

    {:ok,
     assign(socket,
       session: session,
       current_runtime: current_runtime,
       error_message: nil,
       changeset: changeset(current_runtime)
     )}
  end

  defp changeset(runtime, attrs \\ %{}) do
    data =
      case runtime do
        %Runtime.Attached{node: node, cookie: cookie} ->
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
      <div :if={@error_message} class="error-box">
        <%= @error_message %>
      </div>
      <p class="text-gray-700">
        Connect the session to an already running node
        and evaluate code in the context of that node.
        The node must run Erlang/OTP <%= :erlang.system_info(:otp_release) %> and Elixir <%= System.version() %> (or later).
        Make sure to give the node a name and a cookie, for example:
      </p>
      <div class="text-gray-700 markdown">
        <%= if longname = Livebook.Config.longname() do %>
          <pre><code>iex --name test@<%= longname %> --cookie mycookie -S mix</code></pre>
        <% else %>
          <pre><code>iex --sname test --cookie mycookie -S mix</code></pre>
        <% end %>
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
        autocomplete="off"
        spellcheck="false"
      >
        <div class="flex flex-col space-y-4">
          <.text_field field={f[:name]} label="Name" placeholder={name_placeholder()} />
          <.text_field field={f[:cookie]} label="Cookie" placeholder="mycookie" />
        </div>
        <button class="mt-5 button-base button-blue" type="submit" disabled={not @changeset.valid?}>
          <%= if(reconnecting?(@changeset), do: "Reconnect", else: "Connect") %>
        </button>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => data}, socket) do
    changeset =
      socket.assigns.current_runtime |> changeset(data) |> Map.replace!(:action, :validate)

    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("init", %{"data" => data}, socket) do
    socket.assigns.current_runtime
    |> changeset(data)
    |> apply_action(:insert)
    |> case do
      {:ok, data} ->
        node = String.to_atom(data.name)
        cookie = String.to_atom(data.cookie)

        runtime = Runtime.Attached.new(node, cookie)

        case Runtime.connect(runtime) do
          {:ok, runtime} ->
            Session.set_runtime(socket.assigns.session.pid, runtime)
            {:noreply, assign(socket, changeset: changeset(runtime), error_message: nil)}

          {:error, message} ->
            {:noreply,
             assign(socket,
               changeset: changeset(socket.assigns.current_runtime, data),
               error_message: Livebook.Utils.upcase_first(message)
             )}
        end

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end

  @impl true
  def handle_info({:operation, {:set_runtime, _pid, runtime}}, socket) do
    {:noreply, assign(socket, current_runtime: runtime)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp reconnecting?(changeset) do
    changeset.valid? and changeset.data == apply_changes(changeset)
  end

  defp name_placeholder do
    if longname = Livebook.Config.longname(), do: "test@#{longname}", else: "test"
  end
end
