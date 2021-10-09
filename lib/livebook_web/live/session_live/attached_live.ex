defmodule LivebookWeb.SessionLive.AttachedLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Runtime, Utils}

  @impl true
  def mount(_params, %{"session" => session, "current_runtime" => current_runtime}, socket) do
    {:ok,
     assign(socket,
       session: session,
       error_message: nil,
       data: initial_data(current_runtime)
     )}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <%= if @error_message do %>
        <div class="error-box">
          <%= @error_message %>
        </div>
      <% end %>
      <p class="text-gray-700">
        Connect the session to an already running node
        and evaluate code in the context of that node.
        Thanks to this approach you can work with
        an arbitrary Elixir runtime.
        Make sure to give the node a name and a cookie, for example:
      </p>
      <div class="text-gray-700 markdown">
      <%= if longname = Livebook.Config.longname() do %>
        <pre><code>iex --name test@<%= longname %> --cookie mycookie</code></pre>
      <% else %>
        <pre><code>iex --sname test --cookie mycookie</code></pre>
      <% end %>
      </div>
      <p class="text-gray-700">
        Then enter the connection information below:
      </p>
      <.form let={f} for={:data}
        phx-submit="init"
        phx-change="validate"
        autocomplete="off"
        spellcheck="false">
        <div class="flex flex-col space-y-4">
          <div>
            <div class="input-label">Name</div>
            <%= text_input f, :name, value: @data["name"], class: "input", placeholder: name_placeholder() %>
          </div>
          <div>
            <div class="input-label">Cookie</div>
            <%= text_input f, :cookie, value: @data["cookie"], class: "input", placeholder: "mycookie" %>
          </div>
        </div>
        <button class="mt-5 button button-blue"
          type="submit"
          disabled={not data_valid?(@data)}>
          Connect
        </button>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => data}, socket) do
    {:noreply, assign(socket, data: data)}
  end

  def handle_event("init", %{"data" => data}, socket) do
    node = Utils.node_from_name(data["name"])
    cookie = String.to_atom(data["cookie"])

    case Runtime.Attached.init(node, cookie) do
      {:ok, runtime} ->
        Session.connect_runtime(socket.assigns.session.pid, runtime)
        {:noreply, assign(socket, data: data, error_message: nil)}

      {:error, error} ->
        message = runtime_error_to_message(error)
        {:noreply, assign(socket, data: data, error_message: message)}
    end
  end

  defp initial_data(%Runtime.Attached{node: node, cookie: cookie}) do
    %{
      "name" => Atom.to_string(node),
      "cookie" => Atom.to_string(cookie)
    }
  end

  defp initial_data(_runtime), do: %{"name" => "", "cookie" => ""}

  defp data_valid?(data) do
    data["name"] != "" and data["cookie"] != ""
  end

  defp name_placeholder do
    if longname = Livebook.Config.longname(), do: "test@#{longname}", else: "test"
  end

  defp runtime_error_to_message(:unreachable), do: "Node unreachable"
end
