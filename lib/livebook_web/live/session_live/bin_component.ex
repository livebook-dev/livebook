defmodule LivebookWeb.SessionLive.BinComponent do
  use LivebookWeb, :live_component

  alias Livebook.Notebook.Cell

  @initial_limit 10
  @limit_step 10

  @impl true
  def mount(socket) do
    {:ok, assign(socket, search: "", limit: @initial_limit)}
  end

  @impl true
  def update(assigns, socket) do
    {bin_entries, assigns} = Map.pop(assigns, :bin_entries)

    # Only show text cells, as they have an actual content
    bin_entries =
      Enum.filter(bin_entries, fn entry ->
        Cell.type(entry.cell) in [:markdown, :elixir]
      end)

    {:ok,
     socket
     |> assign(:bin_entries, bin_entries)
     |> assign(assigns)
     |> assign_matching_entries()}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 max-w-4xl flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        Bin
      </h3>
      <div class="w-full flex-col space-y-5">
        <p class="text-gray-700">
          Here you can find all the cells deleted within this session.
        </p>
        <%= if @bin_entries == [] do %>
          <div class="p-5 flex space-x-4 items-center border border-gray-200 rounded-lg">
            <div>
              <%= remix_icon("windy-line", class: "text-gray-400 text-xl") %>
            </div>
            <div class="text-gray-600">
              There are currently no cells in the bin.
            </div>
          </div>
        <% else %>
          <form phx-change="search" onsubmit="return false" phx-target="<%= @myself %>">
            <input class="input"
              type="text"
              name="search"
              value="<%= @search %>"
              placeholder="Search"
              autocomplete="off"
              spellcheck="false"
              autofocus />
          </form>
          <div class="flex flex-col space-y-8 overflow-y-auto tiny-scrollbar h-[30rem] pr-3 pb-1">
            <%= for %{cell: cell} = entry <- Enum.take(@matching_entries, @limit) do %>
              <div class="flex flex-col space-y-1">
                <div class="flex justify-between items-center">
                  <p class="text-sm text-gray-700">
                    <span class="font-semibold"><%= Cell.type(cell) |> Atom.to_string() |> String.capitalize() %></span> cell
                    deleted from <span class="font-semibold">“<%= entry.section_name %>”</span> section
                    <span class="font-semibold">
                      <%= entry.deleted_at |> DateTime.to_naive() |> Livebook.Utils.Time.time_ago_in_words() %> ago
                    </span>
                  </p>
                  <div class="flex justify-end space-x-2">
                    <span class="tooltip left" aria-label="Copy source">
                      <button class="icon-button"
                        id="bin-cell-<%= cell.id %>-clipcopy"
                        phx-hook="ClipCopy"
                        data-target-id="bin-cell-<%= cell.id %>-source">
                        <%= remix_icon("clipboard-line", class: "text-lg") %>
                      </button>
                    </span>
                    <span class="tooltip left" aria-label="Restore">
                      <button class="icon-button"
                        phx-click="restore"
                        phx-value-cell_id="<%= entry.cell.id %>"
                        phx-target="<%= @myself %>">
                        <%= remix_icon("arrow-go-back-line", class: "text-lg") %>
                      </button>
                    </span>
                  </div>
                </div>
                <div class="markdown">
                  <pre><code
                    id="bin-cell-<%= cell.id %>-source"
                    phx-hook="Highlight"
                    data-language="<%= Cell.type(cell) %>"><%= cell.source %></code></pre>
                </div>
              </div>
            <% end %>
            <%= if length(@matching_entries) > @limit do %>
              <div class="flex justify-center">
                <button class="button button-outlined-gray" phx-click="more" phx-target="<%= @myself %>">
                  Older
                </button>
              </div>
            <% end %>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("search", %{"search" => search}, socket) do
    {:noreply, assign(socket, search: search, limit: @initial_limit) |> assign_matching_entries()}
  end

  def handle_event("more", %{}, socket) do
    {:noreply, assign(socket, limit: socket.assigns.limit + @limit_step)}
  end

  def handle_event("restore", %{"cell_id" => cell_id}, socket) do
    Livebook.Session.restore_cell(socket.assigns.session_id, cell_id)
    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end

  defp assign_matching_entries(socket) do
    matching_entries = filter_matching(socket.assigns.bin_entries, socket.assigns.search)
    assign(socket, matching_entries: matching_entries)
  end

  defp filter_matching(entries, search) do
    parts =
      search
      |> String.split()
      |> Enum.map(fn part -> ~r/#{Regex.escape(part)}/i end)

    Enum.filter(entries, fn entry ->
      Enum.all?(parts, fn part ->
        entry.cell.source =~ part
      end)
    end)
  end
end
