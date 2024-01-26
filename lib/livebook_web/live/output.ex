defmodule LivebookWeb.Output do
  use LivebookWeb, :html

  alias LivebookWeb.Output

  @doc """
  Renders a single cell output.
  """
  attr :id, :string, required: true
  attr :output, :map, required: true
  attr :session_id, :string, required: true
  attr :session_pid, :any, required: true
  attr :input_views, :map, required: true
  attr :client_id, :string, required: true
  attr :cell_id, :string, required: true

  def output(assigns) do
    ~H"""
    <div id={@id} class="max-w-full" data-el-output data-border={border?(@output)}>
      <%= render_output(@output, %{
        id: "#{@id}-output",
        session_id: @session_id,
        session_pid: @session_pid,
        input_views: @input_views,
        client_id: @client_id,
        cell_id: @cell_id
      }) %>
    </div>
    """
  end

  defp border?(%{type: :terminal_text}), do: true
  defp border?(%{type: :error, context: {:interrupt, _, _}}), do: false
  defp border?(%{type: :error}), do: true
  defp border?(%{type: :grid, boxed: boxed}), do: boxed
  defp border?(_output), do: false

  defp render_output(%{type: :terminal_text, text: text}, %{id: id}) do
    text = if(text == :__pruned__, do: nil, else: text)
    assigns = %{id: id, text: text}

    ~H"""
    <.live_component module={Output.TerminalTextComponent} id={@id} text={@text} />
    """
  end

  defp render_output(%{type: :plain_text, text: text}, %{id: id}) do
    text = if(text == :__pruned__, do: nil, else: text)
    assigns = %{id: id, text: text}

    ~H"""
    <.live_component module={Output.PlainTextComponent} id={@id} text={@text} />
    """
  end

  defp render_output(%{type: :markdown, text: text}, %{id: id, session_id: session_id}) do
    text = if(text == :__pruned__, do: nil, else: text)
    assigns = %{id: id, session_id: session_id, text: text}

    ~H"""
    <.live_component module={Output.MarkdownComponent} id={@id} session_id={@session_id} text={@text} />
    """
  end

  defp render_output(%{type: :image} = output, %{id: id}) do
    assigns = %{id: id, content: output.content, mime_type: output.mime_type}

    ~H"""
    <Output.ImageComponent.render id={@id} content={@content} mime_type={@mime_type} />
    """
  end

  defp render_output(%{type: :js} = output, %{
         id: id,
         session_id: session_id,
         client_id: client_id
       }) do
    assigns = %{
      id: id,
      js_view: output.js_view,
      session_id: session_id,
      client_id: client_id
    }

    ~H"""
    <.live_component
      module={LivebookWeb.JSViewComponent}
      id={@id}
      js_view={@js_view}
      session_id={@session_id}
      client_id={@client_id}
      timeout_message="Output data no longer available, please reevaluate this cell"
    />
    """
  end

  defp render_output(%{type: :frame} = output, %{
         id: id,
         session_id: session_id,
         session_pid: session_pid,
         input_views: input_views,
         client_id: client_id,
         cell_id: cell_id
       }) do
    assigns = %{
      id: id,
      outputs: output.outputs,
      placeholder: output.placeholder,
      session_id: session_id,
      session_pid: session_pid,
      input_views: input_views,
      client_id: client_id,
      cell_id: cell_id
    }

    ~H"""
    <.live_component
      module={Output.FrameComponent}
      id={@id}
      outputs={@outputs}
      placeholder={@placeholder}
      session_id={@session_id}
      session_pid={@session_pid}
      input_views={@input_views}
      client_id={@client_id}
      cell_id={@cell_id}
    />
    """
  end

  defp render_output(%{type: :tabs, outputs: outputs, labels: labels}, %{
         id: id,
         session_id: session_id,
         session_pid: session_pid,
         input_views: input_views,
         client_id: client_id,
         cell_id: cell_id
       }) do
    assigns = %{
      id: id,
      labels: labels,
      outputs: outputs,
      session_id: session_id,
      session_pid: session_pid,
      input_views: input_views,
      client_id: client_id,
      cell_id: cell_id
    }

    ~H"""
    <.live_component
      module={Output.TabsComponent}
      id={@id}
      outputs={@outputs}
      labels={@labels}
      session_id={@session_id}
      session_pid={@session_pid}
      input_views={@input_views}
      client_id={@client_id}
      cell_id={@cell_id}
    />
    """
  end

  defp render_output(%{type: :grid} = grid, %{
         id: id,
         session_id: session_id,
         session_pid: session_pid,
         input_views: input_views,
         client_id: client_id,
         cell_id: cell_id
       }) do
    assigns = %{
      id: id,
      columns: grid.columns,
      gap: grid.gap,
      outputs: grid.outputs,
      session_id: session_id,
      session_pid: session_pid,
      input_views: input_views,
      client_id: client_id,
      cell_id: cell_id
    }

    ~H"""
    <.live_component
      module={Output.GridComponent}
      id={@id}
      outputs={@outputs}
      columns={@columns}
      gap={@gap}
      session_id={@session_id}
      session_pid={@session_pid}
      input_views={@input_views}
      client_id={@client_id}
      cell_id={@cell_id}
    />
    """
  end

  defp render_output(%{type: :input} = input, %{
         id: id,
         input_views: input_views,
         session_pid: session_pid,
         client_id: client_id
       }) do
    assigns = %{
      id: id,
      input: input,
      input_views: input_views,
      session_pid: session_pid,
      client_id: client_id
    }

    ~H"""
    <.live_component
      module={Output.InputComponent}
      id={@id}
      input={@input}
      input_views={@input_views}
      session_pid={@session_pid}
      client_id={@client_id}
    />
    """
  end

  defp render_output(%{type: :control} = control, %{
         id: id,
         input_views: input_views,
         session_pid: session_pid,
         client_id: client_id,
         cell_id: cell_id
       }) do
    assigns = %{
      id: id,
      control: control,
      input_views: input_views,
      session_pid: session_pid,
      client_id: client_id,
      cell_id: cell_id
    }

    ~H"""
    <.live_component
      module={Output.ControlComponent}
      id={@id}
      control={@control}
      input_views={@input_views}
      session_pid={@session_pid}
      client_id={@client_id}
      cell_id={@cell_id}
    />
    """
  end

  defp render_output(
         %{type: :error, context: {:missing_secret, secret_name}} = output,
         %{session_id: session_id, id: id}
       ) do
    assigns = %{message: output.message, secret_name: secret_name, session_id: session_id, id: id}

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
      <%= render_formatted_error_message(@id, @message) %>
    </div>
    """
  end

  defp render_output(
         %{type: :error, context: {:file_entry_forbidden, file_entry_name}} = output,
         %{session_id: session_id, id: id}
       ) do
    assigns = %{
      message: output.message,
      file_entry_name: file_entry_name,
      session_id: session_id,
      id: id
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
      <%= render_formatted_error_message(@id, @message) %>
    </div>
    """
  end

  defp render_output(
         %{type: :error, context: {:interrupt, variant, message}},
         %{cell_id: cell_id}
       ) do
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

  defp render_output(%{type: :error, message: message}, %{id: id}) do
    render_formatted_error_message(id, message)
  end

  defp render_output(output, %{}) do
    req = Livebook.Runtime.Definitions.kino_requirement()

    render_error_message("""
    Unknown output format: #{inspect(output)}. You may want to explicitly \
    add {:kino, "#{req}"} as a notebook dependency or update to the latest \
    Livebook.
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

  defp render_formatted_error_message(id, message) do
    assigns = %{id: id, message: message}

    ~H"""
    <div id={@id} class="relative group/error">
      <div
        id={"#{@id}-message"}
        class="whitespace-pre-wrap break-words font-editor text-gray-500"
        role="complementary"
        aria-label="error"
        phx-no-format
      ><%= LivebookWeb.ANSIHelpers.ansi_string_to_html(@message) %></div>
      <div class="absolute right-2 top-0 z-10 invisible group-hover/error:visible">
        <button
          class="icon-button bg-gray-100"
          phx-click={JS.dispatch("lb:clipcopy", to: "##{@id}-message")}
        >
          <.remix_icon icon="clipboard-line" class="text-lg" />
        </button>
      </div>
    </div>
    """
  end
end
