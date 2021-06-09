defmodule LivebookWeb.SessionLive.MixStandaloneLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Runtime, Utils}

  @type status :: :initial | :initializing | :finished

  @impl true
  def mount(_params, %{"session_id" => session_id, "current_runtime" => current_runtime}, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session_id}")
    end

    {:ok,
     assign(socket,
       session_id: session_id,
       status: :initial,
       current_runtime: current_runtime,
       path: initial_path(current_runtime),
       outputs: [],
       emitter: nil
     ), temporary_assigns: [outputs: []]}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex-col space-y-5">
      <p class="text-gray-700">
        Start a new local node in the context of a Mix project.
        This way all your code and dependencies will be available
        within the notebook.
      </p>
      <p class="text-gray-700">
        <span class="font-semibold">Warning:</span>
        Notebooks that use <code>Mix.install/1</code> do not work
        inside a Mix project because the dependencies of the project
        itself have been installed instead.
      </p>
      <%= if @status == :initial do %>
        <div class="h-full h-52">
          <%= live_component LivebookWeb.PathSelectComponent,
            id: "path_select",
            path: @path,
            extnames: [],
            running_paths: [],
            phx_target: nil,
            phx_submit: if(disabled?(@path), do: nil, else: "init") %>
        </div>
        <%= content_tag :button, if(matching_runtime?(@current_runtime, @path), do: "Reconnect", else: "Connect"),
          class: "button button-blue",
          phx_click: "init",
          disabled: disabled?(@path) %>
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
    emitter = Utils.Emitter.new(self())
    Runtime.MixStandalone.init_async(socket.assigns.path, emitter)
    {:noreply, assign(socket, status: :initializing, emitter: emitter)}
  end

  @impl true
  def handle_info({:emitter, ref, message}, %{assigns: %{emitter: %{ref: ref}}} = socket) do
    case message do
      {:output, output} ->
        {:noreply, add_output(socket, output)}

      {:ok, runtime} ->
        Session.connect_runtime(socket.assigns.session_id, runtime)
        {:noreply, socket |> assign(status: :finished) |> add_output("Connected successfully")}

      {:error, error} ->
        {:noreply, socket |> assign(status: :finished) |> add_output("Error: #{error}")}
    end
  end

  def handle_info({:operation, {:set_runtime, _pid, runtime}}, socket) do
    {:noreply, assign(socket, current_runtime: runtime)}
  end

  def handle_info(_, socket), do: {:noreply, socket}

  defp add_output(socket, output) do
    assign(socket, outputs: socket.assigns.outputs ++ [{output, Utils.random_id()}])
  end

  defp initial_path(%Runtime.MixStandalone{} = current_runtime) do
    current_runtime.project_path
  end

  defp initial_path(_runtime), do: File.cwd!() <> "/"

  defp mix_project_root?(path) do
    File.dir?(path) and File.exists?(Path.join(path, "mix.exs"))
  end

  defp matching_runtime?(%Runtime.MixStandalone{} = runtime, path) do
    Path.expand(runtime.project_path) == Path.expand(path)
  end

  defp matching_runtime?(_runtime, _path), do: false

  defp disabled?(path) do
    not mix_project_root?(path)
  end
end
