defmodule LivebookWeb.SessionLive.AttachedLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Runtime, Utils}

  @impl true
  def mount(_params, %{"session_id" => session_id, "current_runtime" => current_runtime}, socket) do
    {:ok,
     assign(socket,
       session_id: session_id,
       error_message: nil,
       name: if(current_runtime, do: current_runtime.node, else: "")
     )}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex-col space-y-3">
      <%= if @error_message do %>
        <div class="mb-3 rounded-lg px-4 py-2 bg-red-100 text-red-400 font-medium">
          <%= @error_message %>
        </div>
      <% end %>
      <p class="text-gray-700">
        Connect the session to an already running node
        and evaluate code in the context of that node.
        Thanks to this approach you can work with
        an arbitrary Elixir runtime.
        Make sure to give the node a name, for example:
      </p>
      <div class="text-gray-700 markdown">
      <%= if Livebook.Config.shortnames? do %>
        <pre><code>iex --sname test</code></pre>
      <% else %>
        <pre><code>iex --name test@127.0.0.1</code></pre>
      <% end %>
      </div>
      <p class="text-gray-700">
        Then enter the name of the node below:
      </p>
      <%= f = form_for :node, "#", phx_submit: "init" %>
        <%= text_input f, :name, value: @name, class: "input",
              placeholder: if(Livebook.Config.shortnames?, do: "test", else: "test@127.0.0.1") %>

        <%= submit "Connect", class: "mt-3 button" %>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("init", %{"node" => %{"name" => name}}, socket) do
    node = Utils.node_from_name(name)

    case Runtime.Attached.init(node) do
      {:ok, runtime} ->
        Session.connect_runtime(socket.assigns.session_id, runtime)
        {:noreply, assign(socket, name: name, error_message: nil)}

      {:error, error} ->
        message = runtime_error_to_message(error)
        {:noreply, assign(socket, name: name, error_message: message)}
    end
  end

  defp runtime_error_to_message(:unreachable), do: "Node unreachable"

  defp runtime_error_to_message(:already_in_use),
    do: "Another session is already connected to this node"
end
