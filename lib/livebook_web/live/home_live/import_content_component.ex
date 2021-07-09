defmodule LivebookWeb.HomeLive.ImportContentComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, content: "")}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="flex-col space-y-5">
      <p class="text-gray-700">
        Import notebook by directly pasting the <span class="font-semibold">live markdown</span> content.
      </p>
      <.form let={f} for={:data}
        phx-submit="import"
        phx-change="validate"
        phx-target={@myself}
        autocomplete="off">
        <%= textarea f, :content, value: @content, class: "input resize-none",
              placeholder: "Notebook content",
              autofocus: true,
              spellcheck: "false",
              rows: 5 %>
        <button class="mt-5 button button-blue" type="submit" disabled={@content == ""}>
          Import
        </button>
      </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => %{"content" => content}}, socket) do
    {:noreply, assign(socket, content: content)}
  end

  def handle_event("import", %{"data" => %{"content" => content}}, socket) do
    send(self(), {:import_content, content, []})

    {:noreply, socket}
  end
end
