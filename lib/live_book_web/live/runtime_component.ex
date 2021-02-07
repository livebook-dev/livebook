defmodule LiveBookWeb.RuntimeComponent do
  use LiveBookWeb, :live_component

  alias LiveBook.Session
  alias LiveBook.Runtime

  @impl true
  def mount(socket) do
    {:ok, assign(socket, error_message: nil)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6">
      <%= if @error_message do %>
        <div class="mb-3 rounded-md px-4 py-2 bg-red-100 text-red-400 text-sm font-medium">
          <%= @error_message %>
        </div>
      <% end %>
      <h3 class="text-lg font-medium text-gray-900">
        Runtime
      </h3>
      <p class="text-sm text-gray-500">
        The code is evaluated in a separate Elixir runtime (node).
        Here you can configure the node.
      </p>
      <div class="shadow rounded-md p-2 my-4">
        <%= if @runtime do %>
          <table class="w-full text-center text-sm">
            <thead>
              <tr>
                <th class="w-1/3">Type</th>
                <th class="w-1/3">Node name</th>
                <th class="w-1/3"></th>
              </tr>
            </thead>
            <tbody>
              <tr>
                <td><%= runtime_type_label(@runtime) %></td>
                <td><%= @runtime.node %></td>
                <td>
                  <button class="px-3 py-1 bg-white rounded-md border border-red-300 text-sm font-medium text-red-500 hover:bg-red-50"
                    type="button"
                    phx-click="disconnect"
                    phx-target="<%= @myself %>">
                    Disconnect
                  </button>
                </td>
              </tr>
            </tbody>
          </table>
        <% else %>
          <p class="text-sm text-gray-500">
            No connected node
          </p>
        <% end %>
      </div>
      <div class="<%= if @runtime, do: "opacity-50 pointer-events-none" %>">
        <h3 class="text-lg font-medium text-gray-900">
          Standalone node
        </h3>
        <div class="flex-col space-y-3">
          <p class="text-sm text-gray-500">
            You can start a new local node to handle code evaluation.
            This happens automatically as soon as you evaluate the first cell.
          </p>
          <button class="px-4 py-2 bg-white rounded-md border border-gray-300 text-sm font-medium text-gray-700 hover:bg-gray-50"
            type="button"
            phx-click="connect_internal"
            phx-target="<%= @myself %>">
            Connect
          </button>
        </div>
        <h3 class="text-lg font-medium text-gray-900 mt-4">
          Attached node
        </h3>
        <div class="flex-col space-y-3">
          <p class="text-sm text-gray-500">
            You can connect the session to an already running Elixir node
            and evaluate code in the context of that node.
            This is especially handy for developing mix projects.
            Make sure to give the node a name:
          </p>
          <div class="text-sm text-gray-500 markdown">
          <pre><code>iex --name test@127.0.0.1 -S mix</code></pre>
          </div>
          <p class="text-sm text-gray-500">
            Then enter the name of the node below:
          </p>
          <%= f = form_for :node, "#",
            phx_target: @myself,
            phx_submit: "connect_external" %>

            <%= text_input f, :name,
              placeholder: "test@127.0.0.1",
              class: "w-full px-3 py-3 bg-white rounded-md placeholder-gray-400 text-gray-700 text-sm shadow" %>

            <%= submit "Connect",
              class: "mt-3 px-4 py-2 bg-white rounded-md border border-gray-300 text-sm font-medium text-gray-700 hover:bg-gray-50" %>
          </form>
        </div>
      </div>
    </div>
    """
  end

  defp runtime_type_label(%Runtime.Standalone{}), do: "Standalone"
  defp runtime_type_label(%Runtime.Attached{}), do: "Attached"

  @impl true
  def handle_event("disconnect", _params, socket) do
    Session.disconnect(socket.assigns.session_id)

    {:noreply, socket}
  end

  def handle_event("connect_internal", _params, socket) do
    session_pid = Session.get_pid(socket.assigns.session_id)
    handle_runtime_init_result(socket, Runtime.Standalone.init(session_pid))
  end

  def handle_event("connect_external", %{"node" => %{"name" => node}}, socket) do
    node = String.to_atom(node)
    handle_runtime_init_result(socket, Runtime.Attached.init(node))
  end

  defp handle_runtime_init_result(socket, {:ok, runtime}) do
    Session.set_runtime(socket.assigns.session_id, runtime)
    {:noreply, assign(socket, error_message: nil)}
  end

  defp handle_runtime_init_result(socket, {:error, error}) do
    message = runtime_error_to_message(error)
    {:noreply, assign(socket, error_message: message)}
  end

  defp runtime_error_to_message(:unreachable), do: "Node unreachable"
  defp runtime_error_to_message(_), do: "Something went wrong"
end
