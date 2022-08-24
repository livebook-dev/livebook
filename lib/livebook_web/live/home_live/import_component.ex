defmodule LivebookWeb.HomeLive.ImportComponent do
  use LivebookWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Import notebook
      </h3>
      <div class="tabs">
        <%= live_patch to: Routes.home_path(@socket, :import, "url"),
              class: "tab #{if(@tab == "url", do: "active")}" do %>
          <.remix_icon icon="download-cloud-2-line" class="align-middle" />
          <span class="font-medium">
            From URL
          </span>
        <% end %>
        <%= live_patch to: Routes.home_path(@socket, :import, "content"),
              class: "tab #{if(@tab == "content", do: "active")}" do %>
          <.remix_icon icon="clipboard-line" class="align-middle" />
          <span class="font-medium">
            From clipboard
          </span>
        <% end %>
        <%= live_patch to: Routes.home_path(@socket, :import, "file_upload"),
              class: "tab #{if(@tab == "file_upload", do: "active")}" do %>
          <.remix_icon icon="file-upload-line" class="align-middle" />
          <span class="font-medium">
            File upload
          </span>
        <% end %>
        <div class="grow tab"></div>
      </div>
      <div>
        <.live_component module={component_for_tab(@tab)} id={"import-#{@tab}"} {@import_opts} />
      </div>
    </div>
    """
  end

  defp component_for_tab("url"), do: LivebookWeb.HomeLive.ImportUrlComponent
  defp component_for_tab("content"), do: LivebookWeb.HomeLive.ImportContentComponent
  defp component_for_tab("file_upload"), do: LivebookWeb.HomeLive.ImportFileUploadComponent
end
