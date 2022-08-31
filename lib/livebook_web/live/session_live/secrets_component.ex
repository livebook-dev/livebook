defmodule LivebookWeb.SessionLive.SecretsComponent do
  use LivebookWeb, :live_component

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)
    {:ok, socket}
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
          for={:secret}
          phx-submit="save"
          phx-change="validate"
          autocomplete="off"
          phx-target={@myself}
        >
          <div class="flex flex-col space-y-4">
            <div>
              <div class="input-label">
                Label <span class="text-xs text-gray-500">(alphanumeric and underscore)</span>
              </div>
              <%= text_input(f, :label,
                value: @secret["label"],
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
                value: @secret["value"],
                class: "input",
                placeholder: "secret value",
                aria_labelledby: "secret-value",
                spellcheck: "false"
              ) %>
            </div>
          </div>
          <button class="mt-5 button-base button-blue" type="submit" disabled={not valid?(@secret)}>
            Save
          </button>
        </.form>
      </div>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"secret" => secret}, socket) do
    secret = %{label: String.upcase(secret["label"]), value: secret["value"]}
    Livebook.Session.put_secret(socket.assigns.session.pid, secret)
    {:noreply, assign(socket, secret: %{"label" => "", "value" => ""})}
  end

  def handle_event("validate", %{"secret" => secret}, socket) do
    {:noreply, assign(socket, secret: secret)}
  end

  defp valid?(secret) do
    String.match?(secret["label"], ~r/^\w+$/) and secret["value"] != ""
  end
end
