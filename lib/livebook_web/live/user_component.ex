defmodule LivebookWeb.UserComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, data: initial_data())}
  end

  defp initial_data() do
    # TODO: this should default to current user data, whatever it is!
    # TODO: user should always have some color by default, how do we handle that?
    %{"display_name" => "", "color" => random_color()}
  end

  @impl true
  def render(assigns) do
    ~L"""
    <div class="p-6 flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        User config
      </h3>
      <div class="flex justify-center">
        <div class="rounded-full h-20 w-20 flex items-center justify-center" style="background-color: <%= @data["color"] %>">
          <div class="text-3xl text-gray-100 font-semibold">
            <%= avatar_text(@data["display_name"]) %>
          </div>
        </div>
      </div>
      <%= f = form_for :data, "#",
                phx_target: @myself,
                phx_submit: "init",
                phx_change: "validate" %>
        <div class="flex flex-col space-y-5">
          <div>
            <div class="input-label">Display name</div>
            <%= text_input f, :display_name, value: @data["display_name"], class: "input", spellcheck: "false" %>
          </div>
          <div>
            <div class="input-label">Cursor color</div>
            <div class="flex space-x-4 items-center">
              <div class="border-[3px] rounded-lg p-1 flex justify-center items-center"
                style="border-color: <%= @data["color"] %>">
                <div class="rounded h-5 w-5"
                  style="background-color: <%= @data["color"] %>">
                </div>
              </div>
              <div class="relative flex-grow">
                <%= text_input f, :color, value: @data["color"], class: "input", spellcheck: "false", maxlength: 7 %>
                <%= tag :button, class: "icon-button absolute right-2 top-1",
                      type: "button",
                      phx_click: "randomize_color",
                      phx_target: @myself %>
                  <%= remix_icon("refresh-line", class: "text-xl") %>
                </button>
              </div>
            </div>
          </div>
          <%= tag :button, class: "button button-blue flex space-x-1 justify-center items-center",
                type: "submit",
                disabled: not data_valid?(@data) %>
            <%= remix_icon("save-line") %>
            <span>Save</span>
          </button>
        </div>
      </form>
    </div>
    """
  end

  @impl true
  def handle_event("randomize_color", %{}, socket) do
    data = %{socket.assigns.data | "color" => random_color()}
    {:noreply, assign(socket, data: data)}
  end

  def handle_event("validate", %{"data" => data}, socket) do
    {:noreply, assign(socket, data: data)}
  end

  def handle_event("init", %{"data" => data}, socket) do
    {:noreply, assign(socket, data: data)}
  end

  defp data_valid?(data) do
    data["color"] =~ ~r/^#[0-9a-fA-F]{6}$/
  end

  defp random_color() do
    # TODO: use HSV and vary H only? or predefined list of neat colors?
    #   - we want the color to fit white text, so just gather a reasonable list of colors
    # TODO: also color picker
    #   - native picker would trigger change too often, we need either a lib
    #     or something else
    Enum.random(["#F87171", "#FBBF24", "#6EE7B7", "#60A5FA", "#818CF8", "#A78BFA", "#F472B6"])
  end

  defp avatar_text(""), do: "?"

  defp avatar_text(display_name) do
    display_name
    |> String.split()
    |> Enum.map(&String.at(&1, 0))
    |> Enum.map(&String.upcase/1)
    |> case do
      [initial] -> initial
      initials -> List.first(initials) <> List.last(initials)
    end
  end
end
