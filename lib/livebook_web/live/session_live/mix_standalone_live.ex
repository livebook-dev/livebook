defmodule LivebookWeb.SessionLive.MixStandaloneLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Runtime, Utils, FileSystem}

  @type status :: :initial | :initializing | :finished

  @impl true
  def mount(_params, %{"session" => session, "current_runtime" => current_runtime}, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Livebook.PubSub, "sessions:#{session.id}")
    end

    {:ok,
     assign(socket,
       session: session,
       status: :initial,
       current_runtime: current_runtime,
       file: initial_file(current_runtime),
       outputs: [],
       emitter: nil
     ), temporary_assigns: [outputs: []]}
  end

  @impl true
  def render(assigns) do
    ~H"""
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
          <.live_component module={LivebookWeb.FileSelectComponent}
              id="mix-project-dir"
              file={@file}
              extnames={[]}
              running_files={[]}
              submit_event={if(disabled?(@file.path), do: nil, else: :init)}
              file_system_select_disabled={true} />
        </div>
        <button class="button button-blue" phx-click="init" disabled={disabled?(@file.path)}>
          <%= if(matching_runtime?(@current_runtime, @file.path), do: "Reconnect", else: "Connect") %>
        </button>
      <% end %>
      <%= if @status != :initial do %>
        <div class="markdown">
          <pre><code class="max-h-40 overflow-y-auto tiny-scrollbar"
            id="mix-standalone-init-output"
            phx-update="append"
            phx-hook="ScrollOnUpdate"
            ><%= for {output, i} <- @outputs do %><span id={"output-#{i}"}><%= ansi_string_to_html(output) %></span><% end %></code></pre>
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("init", _params, socket) do
    handle_init(socket)
  end

  @impl true
  def handle_info({:set_file, file, _info}, socket) do
    {:noreply, assign(socket, :file, file)}
  end

  def handle_info(:init, socket) do
    handle_init(socket)
  end

  def handle_info({:emitter, ref, message}, %{assigns: %{emitter: %{ref: ref}}} = socket) do
    case message do
      {:output, output} ->
        {:noreply, add_output(socket, output)}

      {:ok, runtime} ->
        Session.connect_runtime(socket.assigns.session.pid, runtime)
        {:noreply, socket |> assign(status: :finished) |> add_output("Connected successfully")}

      {:error, error} ->
        {:noreply, socket |> assign(status: :finished) |> add_output("Error: #{error}")}
    end
  end

  def handle_info({:operation, {:set_runtime, _pid, runtime}}, socket) do
    {:noreply, assign(socket, current_runtime: runtime)}
  end

  def handle_info(_, socket), do: {:noreply, socket}

  defp handle_init(socket) do
    emitter = Utils.Emitter.new(self())
    Runtime.MixStandalone.init_async(socket.assigns.file.path, emitter)
    {:noreply, assign(socket, status: :initializing, emitter: emitter)}
  end

  defp add_output(socket, output) do
    assign(socket, outputs: socket.assigns.outputs ++ [{output, Utils.random_id()}])
  end

  defp initial_file(%Runtime.MixStandalone{} = current_runtime) do
    FileSystem.File.local(current_runtime.project_path)
  end

  defp initial_file(_runtime) do
    Livebook.Config.file_systems()
    |> Enum.find(&is_struct(&1, FileSystem.Local))
    |> FileSystem.File.new()
  end

  defp matching_runtime?(%Runtime.MixStandalone{} = runtime, path) do
    Path.expand(runtime.project_path) == Path.expand(path)
  end

  defp matching_runtime?(_runtime, _path), do: false

  defp disabled?(path) do
    not mix_project_root?(path)
  end

  defp mix_project_root?(path) do
    File.dir?(path) and File.exists?(Path.join(path, "mix.exs"))
  end
end
