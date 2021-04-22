defmodule LivebookWeb.HomeLive.ImportUrlComponent do
  use LivebookWeb, :live_component

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
    |> rewrite_url()
    |> get_content()
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

  # Rewrite known URLs, so that they point to file content rather than HTML page.
  defp rewrite_url(url) do
    url
    |> URI.parse()
    |> do_rewrite_url()
    |> URI.to_string()
  end

  defp do_rewrite_url(%URI{host: "github.com"} = uri) do
    case String.split(uri.path, "/") do
      ["", owner, repo, "blob", hash | file_path] ->
        path = Enum.join(["", owner, repo, hash | file_path], "/")

        %{
          uri
          | path: path,
            host: "raw.githubusercontent.com",
            authority: "raw.githubusercontent.com"
        }

      _ ->
        uri
    end
  end

  defp do_rewrite_url(%URI{host: "gist.github.com"} = uri) do
    case String.split(uri.path, "/") do
      ["", owner, hash] ->
        path = Enum.join(["", owner, hash, "raw"], "/")

        %{
          uri
          | path: path,
            host: "gist.githubusercontent.com",
            authority: "gist.githubusercontent.com"
        }

      _ ->
        uri
    end
  end

  defp do_rewrite_url(uri), do: uri

  defp get_content(url) do
    case :httpc.request(:get, {url, []}, [], body_format: :binary) do
      {:ok, {{_, 200, _}, headers, body}} ->
        valid_content? =
          case fetch_content_type(headers) do
            {:ok, content_type} -> content_type in ["text/plain", "text/markdown"]
            :error -> false
          end

        if valid_content? do
          {:ok, body}
        else
          {:error, "invalid content type, make sure the URL points to live markdown"}
        end

      _ ->
        {:error, "failed to download notebook from the given URL"}
    end
  end

  defp fetch_content_type(headers) do
    case Enum.find(headers, fn {key, _} -> key == 'content-type' end) do
      {_, value} ->
        {:ok,
         value
         |> List.to_string()
         |> String.split(";")
         |> hd()}

      _ ->
        :error
    end
  end
end
