defmodule LivebookWeb.AuthAppListLive do
  use LivebookWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Livebook.Apps.subscribe()
    end

    apps = Livebook.Apps.list_apps()

    {:ok, assign(socket, apps: apps), layout: false}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="w-full flex flex-col space-y-4">
      <.link
        :for={app <- visible_apps(@apps)}
        navigate={~p"/apps/#{app.slug}"}
        class="px-4 py-3 border border-gray-200 rounded-xl text-gray-800 pointer hover:bg-gray-50 flex justify-between"
      >
        <span class="font-semibold"><%= app.notebook_name %></span>
      </.link>
    </div>
    """
  end

  defp visible_apps(apps) do
    apps
    |> Enum.filter(& &1.public?)
    |> Enum.sort_by(& &1.notebook_name)
  end
end
