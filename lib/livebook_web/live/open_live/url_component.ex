defmodule LivebookWeb.OpenLive.UrlComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  @impl true
  def mount(socket) do
    {:ok, assign(socket, changeset: changeset(), error_message: nil)}
  end

  defp changeset(attrs \\ %{}) do
    data = %{url: nil}
    types = %{url: :string}

    cast({data, types}, attrs, [:url])
    |> validate_required([:url])
    |> Livebook.Utils.validate_url(:url, allow_file_scheme: true)
    |> Livebook.Utils.validate_not_s3_url(
      :url,
      ~s{invalid s3:// URL scheme, you must first connect to the Cloud Storage in your Workspace page and then choose the relevant file in "From storage"}
    )
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
        {@error_message}
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
        <div class="mt-5">
          <.button type="submit" disabled={not @changeset.valid?}>
            Import
          </.button>
        </div>
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
        origin = Livebook.Notebook.ContentLoader.url_to_location(data.url)
        files_url = Livebook.Utils.expand_url(data.url, "files/")

        origin
        |> Livebook.Notebook.ContentLoader.fetch_content_from_location()
        |> case do
          {:ok, content} ->
            opts = [origin: origin, files_source: {:url, files_url}]
            send(self(), {:import_source, content, opts})
            socket

          {:error, message} ->
            assign(socket,
              changeset: changeset(data),
              error_message: Livebook.Utils.upcase_first(message)
            )
        end

      {:error, changeset} ->
        assign(socket, changeset: changeset)
    end
  end
end
