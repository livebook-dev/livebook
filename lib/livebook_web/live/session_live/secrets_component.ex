defmodule LivebookWeb.SessionLive.SecretsComponent do
  use LivebookWeb, :live_component

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    {:ok, assign(socket, data: %{"label" => "", "value" => ""}, error_message: nil)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 max-w-4xl flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        Add secret
      </h3>
      <div class="flex-col space-y-5">
        <p class="text-gray-700" id="import-from-url">
          Enter the secret name and its value.
        </p>
        <.form
          let={f}
          for={:data}
          phx-submit="save"
          phx-change="validate"
          phx-target={@myself}
          autocomplete="off"
        >
          <div class="flex flex-col space-y-4">
            <div>
              <div class="input-label">Label</div>
              <%= text_input(f, :label,
                value: @data["label"],
                class: "input",
                placeholder: "secret label",
                autofocus: true,
                aria_labelledby: "secret-label",
                spellcheck: "false"
              ) %>
            </div>
            <div>
              <div class="input-label">Value</div>
              <%= text_input(f, :value,
                value: @data["value"],
                class: "input",
                placeholder: "secret value",
                aria_labelledby: "secret-value",
                spellcheck: "false"
              ) %>
            </div>
          </div>
          <button class="mt-5 button-base button-blue" type="submit" disabled={not data_valid?(@data)}>
            Save
          </button>
        </.form>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"data" => data}, socket) do
    secret = %{label: String.upcase(data["label"]), value: data["value"]}
    Livebook.Session.add_secret(socket.assigns.session.pid, secret)
    {:noreply, assign(socket, data: %{"label" => "", "value" => ""})}
  end

  def handle_event("validate", %{"data" => data}, socket) do
    {:noreply, assign(socket, data: data)}
  end

  defp data_valid?(data) do
    String.match?(data["label"], ~r/^\w+$/) and data["value"] != ""
  end
end
