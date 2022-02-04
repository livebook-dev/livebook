defmodule LivebookWeb.HomeLive.ImportUrlComponent do
  use LivebookWeb, :live_component

  alias Livebook.Utils

  @impl true
  def mount(socket) do
    {:ok, assign(socket, url: "", error_message: nil)}
  end

  @impl true
  def update(assigns, socket) do
    if url = assigns[:url] do
      {:ok, do_import(socket, url)}
    else
      {:ok, socket}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <%= if @error_message do %>
        <div class="error-box">
          <%= @error_message %>
        </div>
      <% end %>
      <p class="text-gray-700" id="import-from-url">
        Paste the URL to a .livemd file, to a GitHub file, or to a Gist to import it.
      </p>
      <.form let={f} for={:data}
        phx-submit="import"
        phx-change="validate"
        phx-target={@myself}
        autocomplete="off">
        <%= text_input f, :url, value: @url, class: "input",
              placeholder: "Notebook URL",
              autofocus: true,
              aria_labelledby: "import-from-url",
              spellcheck: "false" %>
        <button class="mt-5 button-base button-blue"
          type="submit"
          disabled={not Utils.valid_url?(@url)}>
          Import
        </button>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => %{"url" => url}}, socket) do
    {:noreply, assign(socket, url: url)}
  end

  def handle_event("import", %{"data" => %{"url" => url}}, socket) do
    {:noreply, do_import(socket, url)}
  end

  defp do_import(socket, url) do
    origin = Livebook.ContentLoader.url_to_location(url)

    origin
    |> Livebook.ContentLoader.fetch_content_from_location()
    |> case do
      {:ok, content} ->
        send(self(), {:import_content, content, [origin: origin]})
        socket

      {:error, message} ->
        assign(socket, url: url, error_message: Utils.upcase_first(message))
    end
  end
end
