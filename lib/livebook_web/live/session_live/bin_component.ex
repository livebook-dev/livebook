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

    {:ok,
     socket
     |> assign(:bin_entries, bin_entries)
     |> assign(assigns)
     |> assign_matching_entries()}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 max-w-4xl flex flex-col space-y-3">
      <h3 class="text-2xl font-semibold text-gray-800">
        Bin
      </h3>
      <div class="w-full flex-col space-y-5">
        <div class="flex justify-between items-center">
          <p class="text-gray-700 basis-4/5">
            Find all your deleted cells from this notebook session
          </p>
          <%= if @bin_entries != [] do %>
            <form phx-change="search" onsubmit="return false" phx-target={@myself} class="mt-1 relative">
              <span class="absolute inset-y-0 left-0 pl-3 flex items-center">
                <.remix_icon icon="search-line" class="align-bottom text-gray-500" />
              </span>
              <input class="input block w-full pl-10"
                type="text"
                name="search"
                value={@search}
                placeholder="Search"
                autocomplete="off"
                spellcheck="false"
                autofocus />
            </form>
          <% end %>
        </div>
        <%= cond do %>
          <%= @bin_entries == [] -> %>
            <div class="p-5 py-24 flex flex-col gap-3 space-x-4 items-center">
              <div>
                <.remix_icon icon="delete-bin-6-line" class="text-gray-700 text-xl bg-gray-100 p-3 rounded-full" />
              </div>
              <div class="w-64 text-gray-600 text-center">
                You haven't deleted any cells or sections yet. Once you do, they'll appear here.
              </div>
            </div>
          <% @matching_entries == [] -> %>
            <div class="p-5 py-24 flex flex-col gap-3 space-x-4 items-center">
              <div>
                <.remix_icon icon="file-search-line" class="text-gray-700 text-xl bg-gray-100 p-3 rounded-full" />
              </div>
              <div class="w-64 text-gray-600 text-center">
                We couldn't find any results for your query.
              </div>
            </div>
          <% true -> %>
            <div class="flex flex-col space-y-8 min-h-[30rem] pt-3">
              <%= for %{cell: cell} = entry <- Enum.take(@matching_entries, @limit) do %>
                <div class="flex flex-col space-y-1">
                  <div class="flex justify-between items-center">
                    <div class="flex py-1">
                      <.cell_icon cell_type={Cell.type(cell)}/>
                      <p class="text-sm text-gray-700 self-center">
                        <span class="font-semibold"><%= Cell.type(cell) |> Atom.to_string() |> String.capitalize() %></span> cell
                        deleted from <span class="font-semibold">“<%= entry.section_name %>”</span> section
                      </p>
                    </div>
                    <div class="flex justify-end space-x-2">
                      <span class="text-sm text-gray-500">
                        <%= format_date_relatively(entry.deleted_at) %>
                      </span>
                    </div>
                  </div>
                  <.code_preview
                    source_id={"bin-cell-#{cell.id}-source"}
                    language={cell_language(cell)}
                    source={cell.source} />
                  <div class="pt-1 pb-4 border-b border-gray-200">
                    <button class="button-base button-gray whitespace-nowrap py-1 px-2"
                      aria-label="restore"
                      phx-click="restore"
                      phx-value-cell_id={entry.cell.id}
                      phx-target={@myself}>
                      <.remix_icon icon="arrow-go-back-line" class="align-middle mr-1 text-xs" />
                      <span class="font-normal text-xs">Restore</span>
                    </button>
                    <button class="button-base button-gray whitespace-nowrap py-1 px-2"
                      aria-label="copy source"
                      phx-click={JS.dispatch("lb:clipcopy", to: "#bin-cell-#{cell.id}-source")}>
                      <.remix_icon icon="clipboard-line" class="align-middle mr-1 text-xs" />
                      <span class="font-normal text-xs">Copy source</span>
                    </button>
                  </div>
                </div>
              <% end %>
              <%= if length(@matching_entries) > @limit do %>
                <div class="flex justify-center">
                  <button class="button-base button-outlined-gray" phx-click="more" phx-target={@myself}>
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

  defp cell_icon(%{cell_type: :elixir} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 bg-purple-100 rounded items-center justify-center mr-1">
      <svg width="11" height="15" viewBox="0 0 11 15" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path d="M5.7784 3.58083C7.4569 5.87527 9.67878 5.70652 10.0618 9.04833C10.1147 12.9425 8.03684
        14.27 6.55353 14.6441C4.02227 15.3635 1.7644 14.2813 0.875648 11.8316C-0.83154 7.89408 2.36684
        1.41746 4.42502 0.0668945C4.60193 1.32119 5.05745 2.51995 5.75815 3.57521L5.7784 3.58083Z" fill="#663299"/>
      </svg>
    </div>
    """
  end

  defp cell_icon(%{cell_type: :markdown} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 bg-blue-100 rounded items-center justify-center mr-1">
      <svg width="16" height="14" viewBox="0 0 16 14" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path d="M1.25 0.25H14.75C14.9489 0.25 15.1397 0.329018 15.2803 0.46967C15.421 0.610322 15.5 0.801088
        15.5 1V13C15.5 13.1989 15.421 13.3897 15.2803 13.5303C15.1397 13.671 14.9489 13.75 14.75 13.75H1.25C1.05109
        13.75 0.860322 13.671 0.71967 13.5303C0.579018 13.3897 0.5 13.1989 0.5 13V1C0.5 0.801088 0.579018 0.610322
        0.71967 0.46967C0.860322 0.329018 1.05109 0.25 1.25 0.25ZM4.25 9.625V6.625L5.75 8.125L7.25
        6.625V9.625H8.75V4.375H7.25L5.75 5.875L4.25 4.375H2.75V9.625H4.25ZM12.5 7.375V4.375H11V7.375H9.5L11.75
        9.625L14 7.375H12.5Z" fill="#3E64FF"/>
      </svg>
    </div>
    """
  end

  defp cell_icon(%{cell_type: :smart} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 bg-red-100 rounded items-center justify-center mr-1">
      <.remix_icon icon="flashlight-line text-red-900" />
    </div>
    """
  end

  defp cell_language(%Cell.Markdown{}), do: "markdown"
  defp cell_language(%Cell.Elixir{}), do: "elixir"
  defp cell_language(%Cell.Smart{}), do: "elixir"

  defp format_date_relatively(date) do
    time_words = date |> DateTime.to_naive() |> Livebook.Utils.Time.time_ago_in_words()
    time_words <> " ago"
  end

  @impl true
  def handle_event("search", %{"search" => search}, socket) do
    {:noreply, assign(socket, search: search, limit: @initial_limit) |> assign_matching_entries()}
  end

  def handle_event("more", %{}, socket) do
    {:noreply, assign(socket, limit: socket.assigns.limit + @limit_step)}
  end

  def handle_event("restore", %{"cell_id" => cell_id}, socket) do
    Livebook.Session.restore_cell(socket.assigns.session.pid, cell_id)
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
