defmodule LivebookWeb.Output do
  use Phoenix.Component

  @doc """
  Renders the given cell output.
  """
  @spec render_output(Livebook.Cell.Elixir.output(), %{
          id: String.t(),
          socket: Phoenix.LiveView.Socket.t(),
          runtime: Livebook.Runtime.t()
        }) :: Phoenix.LiveView.Rendered.t()
  def render_output(output, context)

  def render_output(text, %{id: id}) when is_binary(text) do
    # Captured output usually has a trailing newline that we can ignore,
    # because each line is itself an HTML block anyway.
    text = String.replace_suffix(text, "\n", "")
    live_component(LivebookWeb.Output.TextComponent, id: id, content: text, follow: true)
  end

  def render_output({:text, text}, %{id: id}) do
    live_component(LivebookWeb.Output.TextComponent, id: id, content: text, follow: false)
  end

  def render_output({:markdown, markdown}, %{id: id}) do
    live_component(LivebookWeb.Output.MarkdownComponent, id: id, content: markdown)
  end

  def render_output({:image, content, mime_type}, %{id: id}) do
    live_component(LivebookWeb.Output.ImageComponent,
      id: id,
      content: content,
      mime_type: mime_type
    )
  end

  def render_output({:vega_lite_static, spec}, %{id: id}) do
    live_component(LivebookWeb.Output.VegaLiteStaticComponent, id: id, spec: spec)
  end

  def render_output({:vega_lite_dynamic, pid}, %{id: id, socket: socket}) do
    live_render(socket, LivebookWeb.Output.VegaLiteDynamicLive,
      id: id,
      session: %{"id" => id, "pid" => pid}
    )
  end

  def render_output({:table_dynamic, pid}, %{id: id, socket: socket}) do
    live_render(socket, LivebookWeb.Output.TableDynamicLive,
      id: id,
      session: %{"id" => id, "pid" => pid}
    )
  end

  def render_output({:frame_dynamic, pid}, %{id: id, socket: socket}) do
    live_render(socket, LivebookWeb.Output.FrameDynamicLive,
      id: id,
      session: %{"id" => id, "pid" => pid}
    )
  end

  def render_output({:error, formatted, :runtime_restart_required}, %{runtime: runtime})
      when runtime != nil do
    assigns = %{formatted: formatted, is_standalone: Livebook.Runtime.standalone?(runtime)}

    ~H"""
    <div class="flex flex-col space-y-4">
    <%= render_error_message_output(@formatted) %>
    <%= if @is_standalone do %>
      <div>
        <button class="button button-gray" phx-click="restart_runtime">
          Restart runtime
        </button>
      </div>
    <% else %>
      <div class="text-red-600">
        <span class="font-semibold">Note:</span>
        This operation requires restarting the runtime, but we cannot
        do it automatically for the current runtime
      </div>
    <% end %>
    </div>
    """
  end

  def render_output({:error, formatted, _type}, %{}) do
    render_error_message_output(formatted)
  end

  def render_output(output, %{}) do
    render_error_message_output("""
    Unknown output format: #{inspect(output)}. If you're using Kino,
    you may want to update Kino and Livebook to the latest version.
    """)
  end

  defp render_error_message_output(message) do
    assigns = %{message: message}

    ~H"""
    <div class="overflow-auto whitespace-pre text-red-600 tiny-scrollbar"><%= @message %></div>
    """
  end
end
