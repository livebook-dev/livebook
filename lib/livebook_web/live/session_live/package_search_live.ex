defmodule LivebookWeb.SessionLive.PackageSearchLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(
        _params,
        %{"session" => session, "runtime" => runtime, "return_to" => return_to},
        socket
      ) do
    socket =
      assign(socket,
        session: session,
        runtime: runtime,
        return_to: return_to,
        search: "",
        search_ref: nil,
        packages: [],
        error_message: nil
      )

    socket = if connected?(socket), do: do_search(socket, ""), else: socket

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Search packages
      </h3>
      <p class="text-gray-700">
        Find external packages for your notebook
      </p>
      <form phx-submit="submit" phx-change="search">
        <input class="input"
          name="search"
          value={@search}
          phx-debounce="250"
          placeholder="Search"
          autocomplete="off"
          spellcheck="false"
          autofocus />
      </form>
      <div class={"flex flex-col divide-y h-[20rem] pr-2 -mr-2 overflow-y-auto tiny-scrollbar #{if @search_ref, do: "opacity-75"}"}>
        <%= cond do %>
          <% @error_message -> %>
            <div class="error-box">
              <%= @error_message %>
            </div>

          <% @packages == [] -> %>
          <div class="flex h-full items-center justify-center text-gray-600">
            <.remix_icon icon="windy-line" class="text-xl" />
            <div class="ml-2">No results</div>
          </div>

        <% true -> %>
          <%= for {package, idx} <- Enum.with_index(@packages) do %>
            <.package package={package} idx={idx} />
          <% end %>
        <% end %>
      </div>
    </div>
    """
  end

  defp package(assigns) do
    ~H"""
    <div class="flex items-center">
      <div class="flex-grow p-2 flex flex-col text-sm">
        <div class="flex text-gray-700">
          <%= if @package[:url] do %>
            <a class="font-semibold" href={@package[:url]} target="_blank"><%= @package.name %></a>
          <% else %>
            <span class="font-semibold"><%= @package.name %></span>
          <% end %>
          <span class="ml-1"><%= @package.version %></span>
        </div>
        <div class="text-gray-600">
          <%= @package.description %>
        </div>
      </div>
      <div class="ml-2">
        <button class="button-base button-gray whitespace-nowrap py-1 px-2"
          aria-label="add"
          phx-click={JS.push("add", value: %{idx: @idx})}>
          <.remix_icon icon="add-line" class="align-middle mr-1 text-xs" />
          <span class="font-normal text-xs">Add</span>
        </button>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("search", %{"search" => search}, socket) do
    {:noreply, do_search(socket, search)}
  end

  @impl true
  def handle_event("submit", %{}, socket) do
    socket =
      case socket.assigns.packages do
        [] -> socket
        [first | _] -> add_dependency(socket, first.dependency)
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("add", %{"idx" => idx}, socket) do
    package = Enum.fetch!(socket.assigns.packages, idx)
    socket = add_dependency(socket, package.dependency)
    {:noreply, socket}
  end

  @impl true
  def handle_info(
        {:runtime_search_packages_response, ref, response},
        %{assigns: %{search_ref: ref}} = socket
      ) do
    socket =
      case response do
        {:ok, packages} ->
          assign(socket, packages: packages, error_message: nil)

        {:error, message} ->
          assign(socket, error_message: Livebook.Utils.upcase_first(message))
      end

    {:noreply, assign(socket, search_ref: nil)}
  end

  def handle_info(_message, socket), do: {:noreply, socket}

  defp do_search(socket, search) do
    search_ref = Livebook.Runtime.search_packages(socket.assigns.runtime, self(), search)
    assign(socket, search_ref: search_ref, search: search)
  end

  defp add_dependency(socket, dependency) do
    Livebook.Session.add_dependencies(socket.assigns.session.pid, [dependency])
    push_patch(socket, to: socket.assigns.return_to)
  end
end
