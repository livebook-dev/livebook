defmodule LivebookWeb.OpenLive.UrlComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  alias Livebook.{Utils, Notebook}

  @impl true
  def mount(socket) do
    {:ok, assign(socket, changeset: changeset(), error_message: nil)}
  end

  defp changeset(attrs \\ %{}) do
    data = %{url: nil}
    types = %{url: :string}

    cast({data, types}, attrs, [:url])
    |> validate_required([:url])
    |> Livebook.Utils.validate_url(:url)
  end

  @impl true
  def update(assigns, socket) do
    if url = assigns[:url] do
      {:ok, do_import(socket, %{url: url})}
    else
      {:ok, socket}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <div :if={@error_message} class="error-box">
        <%= @error_message %>
      </div>
      <p class="text-gray-700" id="import-from-url">
        Paste the URL to a .livemd file, to a GitHub file, or to a Gist.
      </p>
      <.form
        :let={f}
        for={@changeset}
        as={:data}
        phx-submit="import"
        phx-change="validate"
        phx-target={@myself}
        autocomplete="off"
      >
        <.text_field
          field={f[:url]}
          label="Notebook URL"
          autofocus
          aria-labelledby="import-from-url"
          spellcheck="false"
        />
        <button class="mt-5 button-base button-blue" type="submit" disabled={not @changeset.valid?}>
          Import
        </button>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => data}, socket) do
    changeset = data |> changeset() |> Map.replace!(:action, :validate)
    {:noreply, assign(socket, changeset: changeset)}
  end

  def handle_event("import", %{"data" => data}, socket) do
    {:noreply, do_import(socket, data)}
  end

  defp do_import(socket, data) do
    data
    |> changeset()
    |> apply_action(:insert)
    |> case do
      {:ok, data} ->
        origin = Notebook.ContentLoader.url_to_location(data.url)
        files_url = Livebook.Utils.expand_url(data.url, "files/")

        origin
        |> Notebook.ContentLoader.fetch_content_from_location()
        |> case do
          {:ok, content} ->
            opts = [origin: origin, files_source: {:url, files_url}]
            send(self(), {:import_source, content, opts})
            socket

          {:error, message} ->
            assign(socket,
              changeset: changeset(data),
              error_message: Utils.upcase_first(message)
            )
        end

      {:error, changeset} ->
        assign(socket, changeset: changeset)
    end
  end
end
