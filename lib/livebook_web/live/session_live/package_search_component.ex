defmodule LivebookWeb.SessionLive.PackageSearchComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, socket}
  end

  @impl true
  def update(assigns, socket) do
    initial? = socket.assigns[:search] == nil
    socket = assign(socket, assigns)

    socket =
      if initial? do
        socket
        |> assign(
          packages_source: Livebook.Runtime.packages_source(socket.assigns.runtime),
          search: ""
        )
        |> do_search()
      else
        socket
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        Search packages
      </h3>
      <p class="text-gray-700">
        Find external packages for your notebook
      </p>
      <form phx-submit="submit" phx-change="search" phx-target={@myself}>
        <.text_field
          name="search"
          value={@search}
          phx-debounce="250"
          placeholder="Search"
          autocomplete="off"
          spellcheck="false"
          autofocus
          phx-mounted={JS.focus()}
        />
      </form>
      <div class={[
        "flex flex-col divide-y h-80 pr-2 -mr-2 overflow-y-auto tiny-scrollbar",
        if(@packages.loading, do: "opacity-30 transition-opacity duration-300")
      ]}>
        <.async_result :let={packages} assign={@packages}>
          <:loading>
            <div class="flex h-full items-center justify-center text-gray-600">
              <.remix_icon icon="windy-line" class="text-xl" />
              <div class="ml-2">No results</div>
            </div>
          </:loading>
          <:failed :let={{:error, message}}>
            <div class="error-box">
              {Livebook.Utils.upcase_first(message)}
            </div>
          </:failed>
          <div :if={packages == []} class="flex h-full items-center justify-center text-gray-600">
            <.remix_icon icon="windy-line" class="text-xl" />
            <div class="ml-2">No results</div>
          </div>
          <.package :for={{package, idx} <- Enum.with_index(packages)} package={package} idx={idx} />
        </.async_result>
      </div>
    </div>
    """
  end

  defp package(assigns) do
    ~H"""
    <div class="flex items-center">
      <div class="grow p-2 flex flex-col text-sm">
        <div class="flex text-gray-700">
          <%= if @package[:url] do %>
            <a class="font-semibold" href={@package[:url]} target="_blank">{@package.name}</a>
          <% else %>
            <span class="font-semibold">{@package.name}</span>
          <% end %>
          <span class="ml-1">{@package.version}</span>
        </div>
        <div class="text-gray-600">
          {@package.description}
        </div>
      </div>
      <div class="ml-2">
        <.button color="gray" small aria-label="add" phx-click={JS.push("add", value: %{idx: @idx})}>
          <.remix_icon icon="add-line" />
          <span>Add</span>
        </.button>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("search", %{"search" => search}, socket) do
    {:noreply, socket |> assign(search: search) |> do_search()}
  end

  @impl true
  def handle_event("submit", %{}, socket) do
    socket =
      case socket.assigns.packages do
        %{result: [first | _]} -> add_dependency(socket, first.dependency)
        _ -> socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("add", %{"idx" => idx}, socket) do
    package = Enum.fetch!(socket.assigns.packages.result, idx)
    socket = add_dependency(socket, package.dependency)
    {:noreply, socket}
  end

  defp do_search(socket) do
    search = socket.assigns.search
    packages_source = socket.assigns.packages_source

    assign_async(socket, :packages, fn ->
      case packages_source do
        :hex ->
          case Livebook.Runtime.Dependencies.search_packages_on_hex(search) do
            {:ok, packages} -> {:ok, %{packages: packages}}
            {:error, message} -> {:error, message}
          end

        {:list, list} ->
          {:ok, packages} = Livebook.Runtime.Dependencies.search_packages_in_list(list, search)
          {:ok, %{packages: packages}}
      end
    end)
  end

  defp add_dependency(socket, dependency) do
    Livebook.Session.add_dependencies(socket.assigns.session_pid, [dependency])
    assign(socket, search: "", packages: Phoenix.LiveView.AsyncResult.ok([]))
  end
end
