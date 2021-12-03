defmodule LivebookWeb.Output do
  use Phoenix.Component

  @doc """
  Renders a list of cell outputs.
  """
  def outputs(assigns) do
    ~H"""
    <div class="flex flex-col space-y-2">
      <%= for {{outputs, standalone?}, group_idx} <- @outputs |> group_outputs() |> Enum.with_index() do %>
        <div class={"flex flex-col #{if not standalone?, do: "rounded-lg border border-gray-200 divide-y divide-gray-200"}"}>
          <%= for {output, idx} <- Enum.with_index(outputs) do %>
            <div class={"max-w-full #{if not standalone?, do: "px-4"} #{if not composite?(output), do: "py-4"}"}>
              <%= render_output(output, %{
                    id: "#{@id}-output#{group_idx}_#{idx}",
                    socket: @socket,
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

  defp group_outputs(outputs) do
    outputs = Enum.filter(outputs, &(&1 != :ignored))
    group_outputs(outputs, [])
  end

  defp group_outputs([], groups), do: groups

  defp group_outputs([output | outputs], []) do
    group_outputs(outputs, [{[output], standalone?(output)}])
  end

  defp group_outputs([output | outputs], [{group_outputs, group_standalone?} | groups]) do
    case standalone?(output) do
      ^group_standalone? ->
        group_outputs(outputs, [{[output | group_outputs], group_standalone?} | groups])

      standalone? ->
        group_outputs(
          outputs,
          [{[output], standalone?}, {group_outputs, group_standalone?} | groups]
        )
    end
  end

  defp standalone?({:table_dynamic, _}), do: true
  defp standalone?({:frame_dynamic, _}), do: true
  defp standalone?({:input, _}), do: true
  defp standalone?(_output), do: false

  defp composite?({:frame_dynamic, _}), do: true
  defp composite?(_output), do: false

  defp render_output(text, %{id: id}) when is_binary(text) do
    # Captured output usually has a trailing newline that we can ignore,
    # because each line is itself an HTML block anyway.
    text = String.replace_suffix(text, "\n", "")
    live_component(LivebookWeb.Output.TextComponent, id: id, content: text, follow: true)
  end

  defp render_output({:text, text}, %{id: id}) do
    live_component(LivebookWeb.Output.TextComponent, id: id, content: text, follow: false)
  end

  defp render_output({:markdown, markdown}, %{id: id}) do
    live_component(LivebookWeb.Output.MarkdownComponent, id: id, content: markdown)
  end

  defp render_output({:image, content, mime_type}, %{id: id}) do
    live_component(LivebookWeb.Output.ImageComponent,
      id: id,
      content: content,
      mime_type: mime_type
    )
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

  defp render_output({:table_dynamic, pid}, %{id: id, socket: socket}) do
    live_render(socket, LivebookWeb.Output.TableDynamicLive,
      id: id,
      session: %{"id" => id, "pid" => pid}
    )
  end

  defp render_output({:frame_dynamic, pid}, %{id: id, socket: socket, input_values: input_values}) do
    live_render(socket, LivebookWeb.Output.FrameDynamicLive,
      id: id,
      session: %{"id" => id, "pid" => pid, "input_values" => input_values}
    )
  end

  defp render_output({:input, attrs}, %{id: id, input_values: input_values}) do
    live_component(LivebookWeb.Output.InputComponent,
      id: id,
      attrs: attrs,
      input_values: input_values
    )
  end

  defp render_output({:control, attrs}, %{id: id}) do
    live_component(LivebookWeb.Output.ControlComponent, id: id, attrs: attrs)
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
        <button class="button button-gray" phx-click="restart_runtime">
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
