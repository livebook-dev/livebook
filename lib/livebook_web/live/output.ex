defmodule LivebookWeb.Output do
  use LivebookWeb, :html

  import LivebookWeb.Helpers

  alias LivebookWeb.Output

  @doc """
  Renders a list of cell outputs.
  """
  attr :outputs, :list, required: true
  attr :session_id, :string, required: true
  attr :session_pid, :any, required: true
  attr :input_views, :map, required: true
  attr :dom_id_map, :map, required: true
  attr :client_id, :string, required: true
  attr :cell_id, :string, required: true

  def outputs(assigns) do
    ~H"""
    <div
      :for={{idx, output} <- Enum.reverse(@outputs)}
      class="max-w-full"
      id={"output-wrapper-#{@dom_id_map[idx] || idx}"}
      data-el-output
      data-border={border?(output)}
    >
      <%= render_output(output, %{
        id: "output-#{idx}",
        session_id: @session_id,
        session_pid: @session_pid,
        input_views: @input_views,
        client_id: @client_id,
        cell_id: @cell_id
      }) %>
    </div>
    """
  end

  defp border?({:stdout, _text}), do: true
  defp border?({:text, _text}), do: true
  defp border?({:error, _message, {:interrupt, _, _}}), do: false
  defp border?({:error, _message, _type}), do: true
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

  defp render_output({:plain_text, text}, %{id: id}) do
    assigns = %{id: id, text: text}

    ~H"""
    <div id={@id} class="text-gray-700 whitespace-pre-wrap"><%= @text %></div>
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
    <Output.ImageComponent.render id={@id} content={@content} mime_type={@mime_type} />
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

  defp render_output({:frame, outputs, info}, %{
         id: id,
         session_id: session_id,
         session_pid: session_pid,
         input_views: input_views,
         client_id: client_id,
         cell_id: cell_id
       }) do
    live_component(Output.FrameComponent,
      id: id,
      outputs: outputs,
      placeholder: Map.get(info, :placeholder, true),
      session_id: session_id,
      session_pid: session_pid,
      input_views: input_views,
      client_id: client_id,
      cell_id: cell_id
    )
  end

  defp render_output({:tabs, outputs, info}, %{
         id: id,
         session_id: session_id,
         session_pid: session_pid,
         input_views: input_views,
         client_id: client_id,
         cell_id: cell_id
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
      session_id: session_id,
      session_pid: session_pid,
      input_views: input_views,
      client_id: client_id,
      cell_id: cell_id
    }

    # After pruning we don't render labels and we render only those
    # outputs that are kept during pruning

    ~H"""
    <div id={@id}>
      <div class="tabs mb-2" id={"#{@id}-tabs"} phx-update="append">
        <button
          :for={{output_idx, label} <- @labels}
          id={"#{@id}-tabs-#{output_idx}"}
          class={["tab", output_idx == @active_idx && "active"]}
          phx-click={
            JS.remove_class("active", to: "##{@id}-tabs .tab.active")
            |> JS.add_class("active")
            |> JS.add_class("hidden", to: "##{@id}-tab-contents > *:not(.hidden)")
            |> JS.remove_class("hidden", to: "##{@id}-tab-content-#{output_idx}")
          }
        >
          <%= label %>
        </button>
      </div>
      <div id={"#{@id}-tab-contents"} phx-update="append">
        <% # We use data-keep-attribute, because we know active_idx only on the first render %>
        <div
          :for={{output_idx, output} <- @outputs}
          id={"#{@id}-tab-content-#{output_idx}"}
          data-tab-content={output_idx}
          class={[output_idx != @active_idx && "hidden"]}
          data-keep-attribute="class"
        >
          <.outputs
            outputs={[{output_idx, output}]}
            dom_id_map={%{}}
            session_id={@session_id}
            session_pid={@session_pid}
            input_views={@input_views}
            client_id={@client_id}
            cell_id={@cell_id}
          />
        </div>
      </div>
    </div>
    """
  end

  defp render_output({:grid, outputs, info}, %{
         id: id,
         session_id: session_id,
         session_pid: session_pid,
         input_views: input_views,
         client_id: client_id,
         cell_id: cell_id
       }) do
    columns = info[:columns] || 1
    gap = info[:gap] || 8

    assigns = %{
      id: id,
      columns: columns,
      gap: gap,
      outputs: outputs,
      session_id: session_id,
      session_pid: session_pid,
      input_views: input_views,
      client_id: client_id,
      cell_id: cell_id
    }

    ~H"""
    <div id={@id} class="overflow-auto tiny-scrollbar">
      <div
        id={"#{@id}-grid"}
        class="grid grid-cols-2 w-full"
        style={"grid-template-columns: repeat(#{@columns}, minmax(0, 1fr)); gap: #{@gap}px"}
        phx-update="append"
      >
        <div :for={{output_idx, output} <- @outputs} id={"#{@id}-grid-item-#{output_idx}"}>
          <.outputs
            outputs={[{output_idx, output}]}
            dom_id_map={%{}}
            session_id={@session_id}
            session_pid={@session_pid}
            input_views={@input_views}
            client_id={@client_id}
            cell_id={@cell_id}
          />
        </div>
      </div>
    </div>
    """
  end

  defp render_output({:input, attrs}, %{
         id: id,
         input_views: input_views,
         session_pid: session_pid,
         client_id: client_id
       }) do
    live_component(Output.InputComponent,
      id: id,
      attrs: attrs,
      input_views: input_views,
      session_pid: session_pid,
      client_id: client_id
    )
  end

  defp render_output({:control, attrs}, %{
         id: id,
         input_views: input_views,
         session_pid: session_pid,
         client_id: client_id,
         cell_id: cell_id
       }) do
    live_component(Output.ControlComponent,
      id: id,
      attrs: attrs,
      input_views: input_views,
      session_pid: session_pid,
      client_id: client_id,
      cell_id: cell_id
    )
  end

  defp render_output({:error, formatted, {:missing_secret, secret_name}}, %{
         session_id: session_id
       }) do
    assigns = %{
      message: formatted,
      secret_name: secret_name,
      session_id: session_id
    }

    ~H"""
    <div class="-m-4 space-x-4 py-4">
      <div
        class="flex items-center justify-between border-b px-4 pb-4 mb-4"
        style="color: var(--ansi-color-red);"
      >
        <div class="flex space-x-2 font-editor">
          <.remix_icon icon="close-circle-line" />
          <span>Missing secret <%= inspect(@secret_name) %></span>
        </div>
        <.link
          patch={~p"/sessions/#{@session_id}/secrets?secret_name=#{@secret_name}"}
          class="button-base button-gray"
        >
          Add secret
        </.link>
      </div>
      <%= render_formatted_error_message(@message) %>
    </div>
    """
  end

  defp render_output({:error, formatted, {:file_entry_forbidden, file_entry_name}}, %{
         session_id: session_id
       }) do
    assigns = %{
      message: formatted,
      file_entry_name: file_entry_name,
      session_id: session_id
    }

    ~H"""
    <div class="-m-4 space-x-4 py-4">
      <div
        class="flex items-center justify-between border-b px-4 pb-4 mb-4"
        style="color: var(--ansi-color-red);"
      >
        <div class="flex space-x-2 font-editor">
          <.remix_icon icon="close-circle-line" />
          <span>Forbidden access to file <%= inspect(@file_entry_name) %></span>
        </div>
        <button
          class="button-base button-gray"
          phx-click={JS.push("review_file_entry_access", value: %{name: @file_entry_name})}
        >
          Review access
        </button>
      </div>
      <%= render_formatted_error_message(@message) %>
    </div>
    """
  end

  defp render_output({:error, _formatted, {:interrupt, variant, message}}, %{cell_id: cell_id}) do
    assigns = %{variant: variant, message: message, cell_id: cell_id}

    ~H"""
    <div class={[
      "flex justify-between items-center px-4 py-2 border-l-4 shadow-custom-1",
      case @variant do
        :error -> "text-red-400 border-red-400"
        :normal -> "text-gray-500 border-gray-300"
      end
    ]}>
      <div>
        <%= @message %>
      </div>
      <button
        class={[
          "button-base bg-transparent",
          case @variant do
            :error -> "border-red-400 text-red-400 hover:bg-red-50 focus:bg-red-50"
            :normal -> "border-gray-300 text-gray-500 hover:bg-gray-100 focus:bg-gray-100"
          end
        ]}
        phx-click="queue_interrupted_cell_evaluation"
        phx-value-cell_id={@cell_id}
      >
        <.remix_icon icon="play-circle-fill" class="align-middle mr-1" />
        <span>Continue</span>
      </button>
    </div>
    """
  end

  defp render_output({:error, formatted, _type}, %{}) do
    render_formatted_error_message(formatted)
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
      class="whitespace-pre-wrap break-words font-editor text-red-600"
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
      class="whitespace-pre-wrap break-words font-editor text-gray-500"
      role="complementary"
      aria-label="error"
      phx-no-format
    ><%= ansi_string_to_html(@message) %></div>
    """
  end
end
