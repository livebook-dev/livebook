defmodule LivebookWeb.HomeLive.ImportUrlComponent do
  use LivebookWeb, :live_component

  alias Livebook.ContentLoader

  @impl true
  def mount(socket) do
    {:ok, assign(socket, url: "", error_message: nil)}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex-col space-y-5">
      <%= if @error_message do %>
        <div class="mb-3 rounded-lg px-4 py-2 bg-red-100 text-red-400 font-medium">
          <%= @error_message %>
        </div>
      <% end %>
      <p class="text-gray-700">
        Load notebook from an online source (e.g. a GitHub repository)
        simply by pasting its URL.
      </p>
      <%= f = form_for :data, "#",
                phx_submit: "import",
                phx_change: "validate",
                phx_target: @myself,
                autocomplete: "off" %>
        <%= text_input f, :url, value: @url, class: "input",
              placeholder: "Notebook URL",
              spellcheck: "false" %>
        <%= submit "Import",
              class: "mt-5 button button-blue",
              disabled: not valid_url?(@url) %>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => %{"url" => url}}, socket) do
    {:noreply, assign(socket, url: url)}
  end

  def handle_event("import", %{"data" => %{"url" => url}}, socket) do
    url
    |> ContentLoader.rewrite_url()
    |> ContentLoader.fetch_content()
    |> case do
      {:ok, content} ->
        send(self(), {:import_content, content})
        {:noreply, socket}

      {:error, message} ->
        {:noreply, assign(socket, error_message: String.capitalize(message))}
    end
  end

  defp valid_url?(url) do
    uri = URI.parse(url)
    uri.scheme != nil and uri.host != nil and uri.host =~ "."
  end
end
