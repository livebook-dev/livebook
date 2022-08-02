defmodule LivebookWeb.SessionLive.MixStandaloneLive do
  use LivebookWeb, :live_view

  alias Livebook.{Session, Runtime, Utils, FileSystem}

  @type status :: :initial | :initializing | :finished

  @impl true
  def mount(_params, %{"session" => session, "current_runtime" => current_runtime}, socket) do
    unless Livebook.Config.runtime_enabled?(Livebook.Runtime.MixStandalone) do
      raise "runtime module not allowed"
    end

    if connected?(socket) do
      Session.subscribe(session.id)
    end

    {:ok,
     assign(socket,
       session: session,
       status: :initial,
       current_runtime: current_runtime,
       data: initial_data(current_runtime),
       outputs: [],
       outputs_version: 0,
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
        <span class="font-semibold">Warning:</span> Notebooks that use <code>Mix.install/1</code>
        do not work
        inside a Mix project because the dependencies of the project
        itself have been installed instead.
      </p>
      <%= if @status != :initializing do %>
        <div class="h-full h-52">
          <.live_component
            module={LivebookWeb.FileSelectComponent}
            id="mix-project-dir"
            file={@data.file}
            extnames={[]}
            running_files={[]}
            submit_event={if(data_valid?(@data), do: :init, else: nil)}
            file_system_select_disabled={true}
          />
        </div>
        <form phx-change="validate" phx-submit="init">
          <div>
            <div class="input-label"><code>mix run</code> command-line flags</div>
            <input
              class="input"
              type="text"
              name="flags"
              value={@data.flags}
              spellcheck="false"
              autocomplete="off"
            />
          </div>
          <button class="mt-5 button-base button-blue" type="submit" disabled={not data_valid?(@data)}>
            <%= if(matching_runtime?(@current_runtime, @data), do: "Reconnect", else: "Connect") %>
          </button>
        </form>
      <% end %>
      <%= if @status != :initial do %>
        <div class="markdown">
          <pre><code
      class="max-h-40 overflow-y-auto tiny-scrollbar"
      id={"mix-standalone-init-output-#{@outputs_version}"}
      phx-update="append"
      phx-hook="ScrollOnUpdate"
    ><%= for {output, i} <- @outputs do %><span id={
      "mix-standalone-init-output-#{@outputs_version}-#{i}"
    }><%= ansi_string_to_html(output) %></span><% end %></code></pre>
        </div>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("init", _params, socket) do
    handle_init(socket)
  end

  def handle_event("validate", %{"flags" => flags}, socket) do
    {:noreply, update(socket, :data, &%{&1 | flags: flags})}
  end

  @impl true
  def handle_info({:set_file, file, _info}, socket) do
    {:noreply, update(socket, :data, &%{&1 | file: file})}
  end

  def handle_info(:init, socket) do
    handle_init(socket)
  end

  def handle_info({:emitter, ref, message}, %{assigns: %{emitter: %{ref: ref}}} = socket) do
    case message do
      {:output, output} ->
        {:noreply, add_output(socket, output)}

      {:ok, runtime} ->
        Session.set_runtime(socket.assigns.session.pid, runtime)
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
    runtime = Runtime.MixStandalone.new(socket.assigns.data.file.path, socket.assigns.data.flags)
    Runtime.MixStandalone.connect_async(runtime, emitter)

    {:noreply,
     socket
     |> assign(status: :initializing, emitter: emitter, outputs: [])
     |> update(:outputs_version, &(&1 + 1))}
  end

  defp add_output(socket, output) do
    assign(socket, outputs: socket.assigns.outputs ++ [{output, Utils.random_id()}])
  end

  defp initial_data(%Runtime.MixStandalone{project_path: project_path, flags: flags}) do
    file =
      project_path
      |> FileSystem.Utils.ensure_dir_path()
      |> FileSystem.File.local()

    %{file: file, flags: flags}
  end

  defp initial_data(_runtime) do
    %{file: Livebook.Config.local_filesystem_home(), flags: ""}
  end

  defp matching_runtime?(%Runtime.MixStandalone{} = runtime, data) do
    Path.expand(runtime.project_path) == Path.expand(data.file.path)
  end

  defp matching_runtime?(_runtime, _path), do: false

  defp data_valid?(data) do
    mix_project_root?(data.file.path) and Livebook.Utils.valid_cli_flags?(data.flags)
  end

  defp mix_project_root?(path) do
    File.dir?(path) and File.exists?(Path.join(path, "mix.exs"))
  end
end
