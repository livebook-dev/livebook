defmodule LiveBookWeb.SessionLive.MixStandaloneLive do
  use LiveBookWeb, :live_view

  alias LiveBook.{Session, Runtime, Utils}

  @type status :: :initial | :initializing | :finished

  @impl true
  def mount(_params, %{"session_id" => session_id}, socket) do
    {:ok, assign(socket, session_id: session_id, outputs: [], status: :initial, path: default_path()), temporary_assigns: [outputs: []]}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex-col space-y-3">
      <p class="text-gray-500">
        Start a new local node in the context of a mix project.
        This way all your code and dependencies will be available
        within the notebook.
      </p>
      <%= if @status == :initial do %>
        <%= live_component @socket, LiveBookWeb.PathSelectComponent,
          id: "path_select",
          path: @path,
          running_paths: [],
          target: nil %>
        <button class="button-base button-sm" phx-click="init">
          Connect
        </button>
      <% end %>
      <%= if @status != :initial do %>
        <div class="markdown">
          <pre><code class="max-h-40 overflow-y-auto tiny-scrollbar"
            id="mix-standalone-init-output"
            phx-update="append"
            phx-hook="ScrollOnUpdate"
            ><%= for {output, i} <- @outputs do %><span id="output-<%= i %>"><%= ansi_string_to_html(output) %></span><% end %></code></pre>
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("set_path", %{"path" => path}, socket) do
    {:noreply, assign(socket, path: path)}
  end

  def handle_event("init", _params, socket) do
    session_pid = Session.get_pid(socket.assigns.session_id)
    Runtime.MixStandalone.init_async(session_pid, socket.assigns.path)
    {:noreply, assign(socket, status: :initializing)}
  end

  @impl true
  def handle_info({:runtime_init, {:output, output}}, socket) do
    {:noreply, add_output(socket, output)}
  end

  def handle_info({:runtime_init, {:ok, runtime}}, socket) do
    Session.connect_runtime(socket.assigns.session_id, runtime)
    {:noreply, socket |> assign(status: :finished) |> add_output("Connected successfully")}
  end

  def handle_info({:runtime_init, {:error, error}}, socket) do
    {:noreply, socket |> assign(status: :finished) |> add_output("Error: #{error}")}
  end

  defp add_output(socket, output) do
    assign(socket, outputs: socket.assigns.outputs ++ [{output, Utils.random_id()}])
  end

  defp default_path(), do: File.cwd!() <> "/"
end
