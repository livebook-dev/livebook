defmodule LivebookWeb.SessionLive.BinComponent do
  use LivebookWeb, :live_component

  import LivebookWeb.SessionHelpers

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
          <form
            :if={@bin_entries != []}
            phx-change="search"
            phx-nosubmit
            phx-target={@myself}
            class="mt-1 relative"
          >
            <span class="absolute inset-y-0 left-0 pl-3 flex items-center">
              <.remix_icon icon="search-line" class="align-bottom text-gray-500" />
            </span>
            <input
              class="input block w-full pl-10"
              type="text"
              name="search"
              value={@search}
              placeholder="Search"
              autocomplete="off"
              spellcheck="false"
              autofocus
            />
          </form>
        </div>
        <%= cond do %>
          <% @bin_entries == [] -> %>
            <div class="p-5 py-24 flex flex-col gap-3 space-x-4 items-center">
              <div>
                <.remix_icon
                  icon="delete-bin-6-line"
                  class="text-gray-700 text-xl bg-gray-100 p-3 rounded-full"
                />
              </div>
              <div class="w-64 text-gray-600 text-center">
                You haven't deleted any cells or sections yet. Once you do, they'll appear here.
              </div>
            </div>
          <% @matching_entries == [] -> %>
            <div class="p-5 py-24 flex flex-col gap-3 space-x-4 items-center">
              <div>
                <.remix_icon
                  icon="file-search-line"
                  class="text-gray-700 text-xl bg-gray-100 p-3 rounded-full"
                />
              </div>
              <div class="w-64 text-gray-600 text-center">
                We couldn't find any results for your query.
              </div>
            </div>
          <% true -> %>
            <div class="flex flex-col space-y-8 min-h-[30rem] pt-3">
              <div
                :for={%{cell: cell} = entry <- Enum.take(@matching_entries, @limit)}
                class="flex flex-col space-y-1"
              >
                <div class="flex justify-between items-center">
                  <div class="flex py-1">
                    <.cell_icon cell_type={Cell.type(cell)} language={Map.get(cell, :language)} />
                    <p class="ml-1 text-sm text-gray-700 self-center">
                      <span class="font-semibold">
                        <%= Cell.type(cell) |> Atom.to_string() |> String.capitalize() %>
                      </span>
                      cell
                      deleted from <span class="font-semibold">“<%= entry.section_name %>”</span>
                      section
                    </p>
                  </div>
                  <div class="flex justify-end space-x-2">
                    <span class="text-sm text-gray-500">
                      <%= format_datetime_relatively(entry.deleted_at) %> ago
                    </span>
                  </div>
                </div>
                <.code_preview
                  source_id={"bin-cell-#{cell.id}-source"}
                  language={cell_language(cell)}
                  source={cell.source}
                />
                <div class="pt-1 pb-4 border-b border-gray-200">
                  <button
                    class="button-base button-gray whitespace-nowrap py-1 px-2"
                    aria-label="restore"
                    phx-click="restore"
                    phx-value-cell_id={entry.cell.id}
                    phx-target={@myself}
                  >
                    <.remix_icon icon="arrow-go-back-line" class="align-middle mr-1 text-xs" />
                    <span class="font-normal text-xs">Restore</span>
                  </button>
                  <button
                    class="button-base button-gray whitespace-nowrap py-1 px-2"
                    aria-label="copy source"
                    phx-click={JS.dispatch("lb:clipcopy", to: "#bin-cell-#{cell.id}-source")}
                  >
                    <.remix_icon icon="clipboard-line" class="align-middle mr-1 text-xs" />
                    <span class="font-normal text-xs">Copy source</span>
                  </button>
                </div>
              </div>
              <div :if={length(@matching_entries) > @limit} class="flex justify-center">
                <button class="button-base button-outlined-gray" phx-click="more" phx-target={@myself}>
                  Older
                </button>
              </div>
            </div>
        <% end %>
      </div>
    </div>
    """
  end

  defp cell_language(%Cell.Markdown{}), do: "markdown"
  defp cell_language(%Cell.Code{language: language}), do: Atom.to_string(language)
  defp cell_language(%Cell.Smart{}), do: "elixir"

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
