defmodule LivebookWeb.HomeLive.ImportContentComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, content: "")}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="flex-col space-y-5">
      <p class="text-gray-700">
        Import notebook by directly pasting the <span class="font-semibold">live markdown</span> content.
      </p>
      <%= f = form_for :data, "#",
                phx_submit: "import",
                phx_change: "validate",
                phx_target: @myself,
                autocomplete: "off" %>
        <%= textarea f, :content, value: @content, class: "input resize-none",
              placeholder: "Notebook content",
              spellcheck: "false",
              rows: 5 %>

        <%= submit "Import",
              class: "mt-5 button button-blue",
              disabled: @content == "" %>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("validate", %{"data" => %{"content" => content}}, socket) do
    {:noreply, assign(socket, content: content)}
  end

  def handle_event("import", %{"data" => %{"content" => content}}, socket) do
    send(self(), {:import_content, content})

    {:noreply, socket}
  end
end
