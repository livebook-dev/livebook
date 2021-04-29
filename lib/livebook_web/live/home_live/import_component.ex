defmodule LivebookWeb.HomeLive.ImportComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Import notebook
      </h3>
      <div class="tabs">
        <%= live_patch to: Routes.home_path(@socket, :import, "url"),
          class: "tab #{if(@tab == "url", do: "active")}" do %>
          <%= remix_icon("download-cloud-2-line", class: "align-middle") %>
          <span class="font-medium">
            From URL
          </span>
        <% end %>
        <%= live_patch to: Routes.home_path(@socket, :import, "content"),
          class: "tab #{if(@tab == "content", do: "active")}" do %>
          <%= remix_icon("clipboard-line", class: "align-middle") %>
          <span class="font-medium">
            From clipboard
          </span>
        <% end %>
        <div class="flex-grow tab">
        </div>
      </div>
      <div>
        <%= case @tab do %>
          <% "url" -> %>
            <%= live_component @socket, LivebookWeb.HomeLive.ImportUrlComponent,
                  id: "import_url" %>

          <% "content" -> %>
            <%= live_component @socket, LivebookWeb.HomeLive.ImportContentComponent,
                  id: "import_content" %>
        <% end %>
      </div>
    </div>
    """
  end
end
