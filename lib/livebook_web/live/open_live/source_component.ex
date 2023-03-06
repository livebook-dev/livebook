defmodule LivebookWeb.OpenLive.SourceComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, source: "")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <p class="text-gray-700" id="import-from-source">
        Import notebook by directly pasting the <span class="font-semibold">live markdown</span>
        source.
      </p>
      <.form
        :let={f}
        for={%{"source" => @source}}
        as={:data}
        id="import-source"
        phx-submit="import"
        phx-change="validate"
        phx-target={@myself}
        autocomplete="off"
      >
        <.textarea_field
          type="textarea"
          field={f[:source]}
          label="Notebook source"
          resizable={false}
          autofocus
          aria-labelledby="import-from-source"
          spellcheck="false"
          rows="5"
        />
        <button class="mt-5 button-base button-blue" type="submit" disabled={@source == ""}>
          Import
        </button>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => %{"source" => source}}, socket) do
    {:noreply, assign(socket, source: source)}
  end

  def handle_event("import", %{"data" => %{"source" => source}}, socket) do
    send(self(), {:import_source, source, []})

    {:noreply, socket}
  end
end
