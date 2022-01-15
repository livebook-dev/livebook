defmodule LivebookWeb.Output do
  use Phoenix.Component

  @doc """
  Renders a list of cell outputs.
  """
  def outputs(assigns) do
    ~H"""
    <div class="flex flex-col space-y-2">
      <%= for {{output_views, standalone?}, idx} <- @output_views |> group_output_views() |> Enum.with_index() do %>
        <div class={"flex flex-col #{if not standalone?, do: "rounded-lg border border-gray-200 divide-y divide-gray-200"}"}
          id={"outputs-#{@id}-group-#{idx}"}
          phx-update="append">
          <%= for {output_view, output_idx} <- Enum.with_index(output_views), not skip_render?(output_view.output) do %>
            <div class={"max-w-full #{if not standalone?, do: "px-4"} #{if not composite?(output_view.output), do: "py-4"}"}
              id={"outputs-#{@id}-group-#{idx}-#{output_idx}"}>
              <%= render_output(output_view.output, %{
                    id: output_view.id,
                    socket: @socket,
                    session_id: @session_id,
                    runtime: @runtime,
                    cell_validity_status: @cell_validity_status,
                    input_values: @input_values
                  }) %>
            </div>
          <% end %>
        </div>
      <% end %>
    </div>
    """
  end

  defp group_output_views(output_views) do
    output_views = Enum.reject(output_views, &match?(%{output: :ignored}, &1))
    group_output_views(output_views, [])
  end

  defp group_output_views([], groups), do: groups

  defp group_output_views([view | views], []) do
    group_output_views(views, [{[view], standalone?(view.output)}])
  end

  defp group_output_views([view | views], [{group_views, group_standalone?} | groups]) do
    case standalone?(view.output) do
      ^group_standalone? ->
        group_output_views(views, [{[view | group_views], group_standalone?} | groups])

      standalone? ->
        group_output_views(
          views,
          [{[view], standalone?}, {group_views, group_standalone?} | groups]
        )
    end
  end

  defp standalone?(:ignored), do: false
  defp standalone?({:stdout, _text}), do: false
  defp standalone?({:text, _text}), do: false
  defp standalone?({:error, _message, _type}), do: false
  defp standalone?(_output), do: true

  defp composite?({:frame, _outputs, _info}), do: true
  defp composite?(_output), do: false

  defp skip_render?({:stdout, :__pruned__}), do: true
  defp skip_render?({:text, :__pruned__}), do: true
  defp skip_render?({:image, :__pruned__, :__pruned__}), do: true
  defp skip_render?({:markdown, :__pruned__}), do: true
  defp skip_render?(_output), do: false

  defp render_output({:stdout, text}, %{id: id}) do
    text = if(text == :__pruned__, do: nil, else: text)
    live_component(LivebookWeb.Output.StdoutComponent, id: id, text: text, follow: true)
  end

  defp render_output({:text, text}, %{id: id}) do
    assigns = %{id: id, text: text}

    ~H"""
    <LivebookWeb.Output.TextComponent.render id={@id} content={@text} follow={false} />
    """
  end

  defp render_output({:markdown, markdown}, %{id: id}) do
    live_component(LivebookWeb.Output.MarkdownComponent, id: id, content: markdown)
  end

  defp render_output({:image, content, mime_type}, %{id: id}) do
    assigns = %{id: id, content: content, mime_type: mime_type}

    ~H"""
    <LivebookWeb.Output.ImageComponent.render content={@content} mime_type={@mime_type} />
    """
  end

  defp render_output({:vega_lite_static, spec}, %{id: id}) do
    live_component(LivebookWeb.Output.VegaLiteStaticComponent, id: id, spec: spec)
  end

  defp render_output({:vega_lite_dynamic, pid}, %{id: id, socket: socket}) do
    live_render(socket, LivebookWeb.Output.VegaLiteDynamicLive,
      id: id,
      session: %{"id" => id, "pid" => pid}
    )
  end

  defp render_output({:js, info}, %{id: id, session_id: session_id}) do
    live_component(LivebookWeb.Output.JSComponent, id: id, info: info, session_id: session_id)
  end

  defp render_output({:table_dynamic, pid}, %{id: id, socket: socket}) do
    live_render(socket, LivebookWeb.Output.TableDynamicLive,
      id: id,
      session: %{"id" => id, "pid" => pid}
    )
  end

  defp render_output({:frame_dynamic, pid}, %{
         id: id,
         socket: socket,
         session_id: session_id,
         input_values: input_values,
         cell_validity_status: cell_validity_status
       }) do
    live_render(socket, LivebookWeb.Output.FrameDynamicLive,
      id: id,
      session: %{
        "id" => id,
        "pid" => pid,
        "session_id" => session_id,
        "input_values" => input_values,
        "cell_validity_status" => cell_validity_status
      }
    )
  end

  defp render_output({:frame, outputs, _info}, %{
         id: id,
         input_values: input_values,
         session_id: session_id
       }) do
    live_component(LivebookWeb.Output.FrameComponent,
      id: id,
      outputs: outputs,
      session_id: session_id,
      input_values: input_values
    )
  end

  defp render_output({:input, attrs}, %{id: id, input_values: input_values}) do
    live_component(LivebookWeb.Output.InputComponent,
      id: id,
      attrs: attrs,
      input_values: input_values
    )
  end

  defp render_output({:control, attrs}, %{id: id, input_values: input_values}) do
    live_component(LivebookWeb.Output.ControlComponent,
      id: id,
      attrs: attrs,
      input_values: input_values
    )
  end

  defp render_output({:error, formatted, :runtime_restart_required}, %{
         runtime: runtime,
         cell_validity_status: cell_validity_status
       })
       when runtime != nil and cell_validity_status == :evaluated do
    assigns = %{formatted: formatted, is_standalone: Livebook.Runtime.standalone?(runtime)}

    ~H"""
    <div class="flex flex-col space-y-4">
      <%= render_error_message_output(@formatted) %>
      <%= if @is_standalone do %>
        <div>
          <button class="button-base button-gray" phx-click="restart_runtime">
            Reconnect runtime
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

  defp render_output({:error, formatted, _type}, %{}) do
    render_error_message_output(formatted)
  end

  defp render_output(output, %{}) do
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
