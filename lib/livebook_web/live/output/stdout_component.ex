defmodule LivebookWeb.Output.StdoutComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, modifiers: [], last_line: nil, last_html_line: nil),
     temporary_assigns: [html_lines: []]}
  end

  @impl true
  def update(assigns, socket) do
    {text, assigns} = Map.pop(assigns, :text)
    socket = assign(socket, assigns)

    if text do
      text = (socket.assigns.last_line || "") <> text

      text = Livebook.Notebook.normalize_stdout(text)

      last_line =
        case Livebook.Utils.split_at_last_occurrence(text, "\n") do
          :error -> text
          {:ok, _, last_line} -> last_line
        end

      {html_lines, modifiers} =
        LivebookWeb.Helpers.ANSI.ansi_string_to_html_lines_step(text, socket.assigns.modifiers)

      {html_lines, [last_html_line]} = Enum.split(html_lines, -1)

      {:ok,
       assign(socket,
         html_lines: html_lines,
         last_html_line: last_html_line,
         last_line: last_line,
         modifiers: modifiers
       )}
    else
      {:ok, socket}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id={"virtualized-text-#{@id}"}
      class="relative"
      phx-hook="VirtualizedLines"
      data-max-height="300"
      data-follow="true"
      data-max-lines={Livebook.Notebook.max_stdout_lines()}
      data-ignore-trailing-empty-line="true">
      <%# Note 1: We add a newline to each element, so that multiple lines can be copied properly as element.textContent %>
      <%# Note 2: We use comments to avoid inserting unintended whitespace %>
      <div data-template class="hidden" id={"virtualized-text-#{@id}-template"}><%#
      %><div id={"virtualized-text-#{@id}-template-append"} phx-update="append"><%#
        %><%= for html_line <- @html_lines do %><%#
          %><div data-line id={Livebook.Utils.random_id()}><%= [html_line, "\n"] %></div><%#
        %><% end %><%#
      %></div><%#
      %><div data-line><%= @last_html_line %></div><%#
    %></div>
      <div data-content class="overflow-auto whitespace-pre font-editor text-gray-500 tiny-scrollbar"
        id={"virtualized-text-#{@id}-content"}
        phx-update="ignore"></div>
      <div class="absolute right-2 top-0 z-10">
        <button class="icon-button bg-gray-100"
          data-el-clipcopy
          phx-click={JS.dispatch("lb:clipcopy", to: "#virtualized-text-#{@id}-template")}>
          <.remix_icon icon="clipboard-line" class="text-lg" />
        </button>
      </div>
    </div>
    """
  end
end
