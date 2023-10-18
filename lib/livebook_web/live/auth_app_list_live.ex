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
        href={~p"/apps/#{app.slug}"}
        class="px-4 py-3 border border-gray-200 rounded-xl text-gray-800 pointer hover:bg-gray-50 flex items-center justify-between"
      >
        <span class="font-semibold"><%= app.notebook_name %></span>
        <.remix_icon :if={not app.public?} icon="lock-password-line" />
      </.link>
    </div>
    """
  end

  defp visible_apps(apps) do
    Enum.sort_by(apps, & &1.notebook_name)
  end
end
