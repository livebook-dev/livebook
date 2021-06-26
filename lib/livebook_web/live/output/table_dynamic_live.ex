defmodule LivebookWeb.Output.TableDynamicLive do
  use LivebookWeb, :live_view

  @limit 10
  @loading_delay_ms 100

  @impl true
  def mount(_params, %{"pid" => pid, "id" => id}, socket) do
    send(pid, {:connect, self()})

    {:ok,
     assign(socket,
       id: id,
       pid: pid,
       loading: true,
       show_loading_timer: nil,
       # Data specification
       page: 1,
       limit: @limit,
       order_by: nil,
       order: :asc,
       # Fetched data
       name: "Table",
       features: [],
       columns: [],
       rows: [],
       total_rows: 0
     )}
  end

  @impl true
  def render(%{loading: true} = assigns) do
    ~L"""
    <div class="max-w-2xl w-full animate-pulse">
      <div class="flex-1 space-y-4">
        <div class="h-4 bg-gray-200 rounded-lg w-3/4"></div>
        <div class="h-4 bg-gray-200 rounded-lg"></div>
        <div class="h-4 bg-gray-200 rounded-lg w-5/6"></div>
      </div>
    </div>
    """
  end

  def render(assigns) do
    ~L"""
    <div class="mb-4 flex items-center space-x-3">
      <h3 class="font-semibold text-gray-800">
        <%= @name %>
      </h3>
      <div class="flex-grow"></div>
      <!-- Actions -->
      <div class="flex space-x-2">
        <%= if :refetch in @features do %>
          <span class="tooltip left" aria-label="Refetch">
            <%= tag :button, class: "icon-button",
                  phx_click: "refetch" %>
              <%= remix_icon("refresh-line", class: "text-xl") %>
            </button>
          </span>
        <% end %>
      </div>
      <!-- Pagination -->
      <%= if :pagination in @features and @total_rows > 0 do %>
        <div class="flex space-x-2">
          <%= tag :button,
                class: "flex items-center font-medium text-sm text-gray-400 hover:text-gray-800 disabled:pointer-events-none disabled:text-gray-300",
                disabled: @page == 1,
                phx_click: "prev" %>
            <%= remix_icon("arrow-left-s-line", class: "text-xl") %>
            <span>Prev</span>
          </button>
          <div class="flex items-center px-3 py-1 rounded-lg border border-gray-300 font-medium text-sm text-gray-400">
            <span><%= @page %> of <%= max_page(@total_rows, @limit) %></span>
          </div>
          <%= tag :button,
                class: "flex items-center font-medium text-sm text-gray-400 hover:text-gray-800 disabled:pointer-events-none disabled:text-gray-300",
                disabled: @page == max_page(@total_rows, @limit),
                phx_click: "next" %>
            <span>Next</span>
            <%= remix_icon("arrow-right-s-line", class: "text-xl") %>
          </button>
        </div>
      <% end %>
    </div>
    <%= if @columns == [] do %>
      <!-- In case we don't have information about table structure yet -->
      <p class="text-gray-700">
        No data
      </p>
    <% else %>
      <!-- Data table -->
      <div class="shadow-xl-center rounded-lg max-w-full overflow-y-auto tiny-scrollbar">
        <table class="w-full">
          <thead class="text-left">
            <tr class="border-b border-gray-200 whitespace-nowrap">
              <%= for {column, idx} <- Enum.with_index(@columns) do %>
                <th class="py-3 px-6 text-gray-700 font-semibold <%= if(:sorting in @features, do: "cursor-pointer", else: "pointer-events-none") %>"
                  phx-click="column_click"
                  phx-value-column_idx="<%= idx %>">
                  <div class="flex items-center space-x-1">
                    <span><%= column.label %></span>
                    <%= tag :span, class: unless(@order_by == column.key, do: "invisible") %>
                      <%= remix_icon(order_icon(@order), class: "text-xl align-middle leading-none") %>
                    </span>
                  </div>
                </th>
              <% end %>
            </tr>
          </thead>
          <tbody class="text-gray-500">
            <%= for row <- @rows do %>
              <tr class="border-b border-gray-200 last:border-b-0 hover:bg-gray-50 whitespace-nowrap">
                <%= for column <- @columns do %>
                  <td class="py-3 px-6">
                    <%= to_string(row.fields[column.key]) %>
                  </td>
                <% end %>
              </tr>
            <% end %>
          </tbody>
        </table>
      </div>
    <% end %>
    """
  end

  defp order_icon(:asc), do: "arrow-up-s-line"
  defp order_icon(:desc), do: "arrow-down-s-line"

  defp max_page(total_rows, limit) do
    ceil(total_rows / limit)
  end

  @impl true
  def handle_event("refetch", %{}, socket) do
    {:noreply, request_rows(socket)}
  end

  def handle_event("prev", %{}, socket) do
    {:noreply, assign(socket, :page, socket.assigns.page - 1) |> request_rows()}
  end

  def handle_event("next", %{}, socket) do
    {:noreply, assign(socket, :page, socket.assigns.page + 1) |> request_rows()}
  end

  def handle_event("column_click", %{"column_idx" => idx}, socket) do
    idx = String.to_integer(idx)
    %{key: key} = Enum.at(socket.assigns.columns, idx)

    {order_by, order} =
      case {socket.assigns.order_by, socket.assigns.order} do
        {^key, :asc} -> {key, :desc}
        {^key, :desc} -> {nil, :asc}
        _ -> {key, :asc}
      end

    {:noreply, assign(socket, order_by: order_by, order: order) |> request_rows()}
  end

  @impl true
  def handle_info({:connect_reply, %{name: name, columns: columns, features: features}}, socket) do
    {:noreply, assign(socket, name: name, columns: columns, features: features) |> request_rows()}
  end

  def handle_info({:rows, %{rows: rows, total_rows: total_rows, columns: columns}}, socket) do
    columns =
      case columns do
        :initial -> socket.assigns.columns
        columns when is_list(columns) -> columns
      end

    if socket.assigns.show_loading_timer do
      Process.cancel_timer(socket.assigns.show_loading_timer)
    end

    {:noreply,
     assign(socket,
       loading: false,
       show_loading_timer: nil,
       columns: columns,
       rows: rows,
       total_rows: total_rows
     )}
  end

  def handle_info(:show_loading, socket) do
    {:noreply, assign(socket, loading: true, show_loading_timer: nil)}
  end

  defp request_rows(socket) do
    rows_spec = %{
      offset: (socket.assigns.page - 1) * socket.assigns.limit,
      limit: socket.assigns.limit,
      order_by: socket.assigns.order_by,
      order: socket.assigns.order
    }

    send(socket.assigns.pid, {:get_rows, self(), rows_spec})

    show_loading_timer = Process.send_after(self(), :show_loading, @loading_delay_ms)
    assign(socket, show_loading_timer: show_loading_timer)
  end
end
