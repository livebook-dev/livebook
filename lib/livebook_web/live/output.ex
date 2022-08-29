defmodule LivebookWeb.Output do
  use Phoenix.Component

  import LivebookWeb.Helpers

  alias Phoenix.LiveView.JS
  alias LivebookWeb.Output
  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Renders a list of cell outputs.
  """
  def outputs(assigns) do
    ~H"""
    <%= for {idx, output} <- Enum.reverse(@outputs) do %>
      <div
        class="max-w-full"
        id={"output-wrapper-#{@dom_id_map[idx] || idx}"}
        data-el-output
        data-border={border?(output)}
      >
        <%= render_output(output, %{
          id: "output-#{idx}",
          socket: @socket,
          session_id: @session_id,
          input_values: @input_values,
          client_id: @client_id
        }) %>
      </div>
    <% end %>
    """
  end

  defp border?({:stdout, _text}), do: true
  defp border?({:text, _text}), do: true
  defp border?({:error, _message}), do: true
  defp border?({:grid, _, info}), do: Map.get(info, :boxed, false)
  defp border?(_output), do: false

  defp render_output({:stdout, text}, %{id: id}) do
    text = if(text == :__pruned__, do: nil, else: text)
    live_component(Output.StdoutComponent, id: id, text: text)
  end

  defp render_output({:text, text}, %{id: id}) do
    assigns = %{id: id, text: text}

    ~H"""
    <Output.TextComponent.render id={@id} content={@text} />
    """
  end

  defp render_output({:markdown, markdown}, %{id: id, session_id: session_id}) do
    live_component(Output.MarkdownComponent,
      id: id,
      session_id: session_id,
      content: markdown
    )
  end

  defp render_output({:image, content, mime_type}, %{id: id}) do
    assigns = %{id: id, content: content, mime_type: mime_type}

    ~H"""
    <Output.ImageComponent.render content={@content} mime_type={@mime_type} />
    """
  end

  defp render_output({:js, js_info}, %{id: id, session_id: session_id, client_id: client_id}) do
    live_component(LivebookWeb.JSViewComponent,
      id: id,
      js_view: js_info.js_view,
      session_id: session_id,
      client_id: client_id,
      timeout_message: "Output data no longer available, please reevaluate this cell"
    )
  end

  defp render_output({:frame, outputs, _info}, %{
         id: id,
         session_id: session_id,
         input_values: input_values,
         client_id: client_id
       }) do
    live_component(Output.FrameComponent,
      id: id,
      outputs: outputs,
      session_id: session_id,
      input_values: input_values,
      client_id: client_id
    )
  end

  defp render_output({:tabs, outputs, info}, %{
         id: id,
         socket: socket,
         session_id: session_id,
         input_values: input_values,
         client_id: client_id
       }) do
    {labels, active_idx} =
      if info.labels == :__pruned__ do
        {[], nil}
      else
        labels =
          Enum.zip_with(info.labels, outputs, fn label, {output_idx, _} -> {output_idx, label} end)

        active_idx = get_in(outputs, [Access.at(0), Access.elem(0)])

        {labels, active_idx}
      end

    assigns = %{
      id: id,
      active_idx: active_idx,
      labels: labels,
      outputs: outputs,
      socket: socket,
      session_id: session_id,
      input_values: input_values,
      client_id: client_id
    }

    # After pruning we don't render labels and we render only those
    # outputs that are kept during pruning

    ~H"""
    <div id={@id}>
      <div class="tabs mb-2" id={"#{@id}-tabs"} phx-update="append">
        <%= for {output_idx, label} <- @labels do %>
          <button
            id={"#{@id}-tabs-#{output_idx}"}
            class={"tab #{if(output_idx == @active_idx, do: "active")}"}
            phx-click={
              JS.remove_class("active", to: "##{@id}-tabs .tab.active")
              |> JS.add_class("active")
              |> JS.add_class("hidden", to: "##{@id}-tab-contents > *:not(.hidden)")
              |> JS.remove_class("hidden", to: "##{@id}-tab-content-#{output_idx}")
            }
          >
            <%= label %>
          </button>
        <% end %>
      </div>
      <div id={"#{@id}-tab-contents"} phx-update="append">
        <%= for {output_idx, output} <- @outputs do %>
          <% # We use data-keep-attribute, because we know active_idx only on the first render %>
          <div
            id={"#{@id}-tab-content-#{output_idx}"}
            data-tab-content={output_idx}
            class={"#{if(output_idx != @active_idx, do: "hidden")}"}
            data-keep-attribute="class"
          >
            <.outputs
              outputs={[{output_idx, output}]}
              dom_id_map={%{}}
              socket={@socket}
              session_id={@session_id}
              input_values={@input_values}
              client_id={@client_id}
            />
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  defp render_output({:grid, outputs, info}, %{
         id: id,
         session_id: session_id,
         socket: socket,
         input_values: input_values,
         client_id: client_id
       }) do
    columns = info[:columns] || 1
    gap = info[:gap] || 8

    assigns = %{
      id: id,
      columns: columns,
      gap: gap,
      outputs: outputs,
      socket: socket,
      session_id: session_id,
      input_values: input_values,
      client_id: client_id
    }

    ~H"""
    <div id={@id} class="overflow-auto tiny-scrollbar">
      <div
        id={"#{@id}-grid"}
        class="grid grid-cols-2 w-full"
        style={"grid-template-columns: repeat(#{@columns}, minmax(0, 1fr)); gap: #{@gap}px"}
        phx-update="append"
      >
        <%= for {output_idx, output} <- @outputs do %>
          <div id={"#{@id}-grid-item-#{output_idx}"}>
            <.outputs
              outputs={[{output_idx, output}]}
              dom_id_map={%{}}
              socket={@socket}
              session_id={@session_id}
              input_values={@input_values}
              client_id={@client_id}
            />
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  defp render_output({:input, attrs}, %{id: id, input_values: input_values, client_id: client_id}) do
    live_component(Output.InputComponent,
      id: id,
      attrs: attrs,
      input_values: input_values,
      client_id: client_id
    )
  end

  defp render_output({:control, attrs}, %{
         id: id,
         input_values: input_values,
         client_id: client_id
       }) do
    live_component(Output.ControlComponent,
      id: id,
      attrs: attrs,
      input_values: input_values,
      client_id: client_id
    )
  end

  defp render_output({:error, formatted, {:unavailable_env, "LB_" <> _rest}}, %{
         socket: socket,
         session_id: session_id
       }) do
    assigns = %{message: String.replace(formatted, ~s(environment variable "LB_), ~s(secret "))}

    ~H"""
    <div class="flex justify-between">
      <%= render_formatted_error_message(@message) %>
      <div class="self-end">
        <%= live_patch to: Routes.session_path(socket, :secrets, session_id),
            class: "button-base button-gray",
            aria_label: "add secret",
            role: "button" do %>
          <span>Add secret</span>
        <% end %>
      </div>
    </div>
    """
  end

  defp render_output({:error, formatted, _type}, %{}) do
    render_formatted_error_message(formatted)
  end

  # TODO: remove on Livebook v0.7
  defp render_output(output, %{})
       when elem(output, 0) in [
              :vega_lite_static,
              :vega_lite_dynamic,
              :table_dynamic,
              :frame_dynamic
            ] do
    render_error_message("""
    Legacy output format: #{inspect(output)}. Please update Kino to
    the latest version.
    """)
  end

  defp render_output(output, %{}) do
    render_error_message("""
    Unknown output format: #{inspect(output)}. If you're using Kino,
    you may want to update Kino and Livebook to the latest version.
    """)
  end

  defp render_error_message(message) do
    assigns = %{message: message}

    ~H"""
    <div
      class="whitespace-pre-wrap font-editor text-red-600"
      role="complementary"
      aria-label="error message"
      phx-no-format
    ><%= @message %></div>
    """
  end

  defp render_formatted_error_message(formatted) do
    assigns = %{message: formatted}

    ~H"""
    <div
      class="whitespace-pre-wrap font-editor text-gray-500"
      role="complementary"
      aria-label="error"
      phx-no-format
    ><%= ansi_string_to_html(@message) %></div>
    """
  end
end
