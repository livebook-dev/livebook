defmodule LivebookWeb.SessionLive.SecretsComponent do
  use LivebookWeb, :live_component

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, assigns)

    socket =
      if socket.assigns[:data] do
        socket
      else
        assign(socket, data: %{"label" => prefill_secret_label(socket), "value" => ""})
      end

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 max-w-4xl flex flex-col space-y-5">
      <h3 class="text-2xl font-semibold text-gray-800">
        Add secret
      </h3>
      <p class="text-gray-700">
        Enter the secret name and its value.
      </p>
      <.form
        let={f}
        for={:data}
        phx-submit="save"
        phx-change="validate"
        autocomplete="off"
        phx-target={@myself}
        errors={data_errors(@data)}
      >
        <div class="flex flex-col space-y-4">
          <.input_wrapper form={f} field={:label}>
            <div class="input-label">
              Label <span class="text-xs text-gray-500">(alphanumeric and underscore)</span>
            </div>
            <%= text_input(f, :label,
              value: @data["label"],
              class: "input",
              autofocus: !@prefill_secret_label,
              spellcheck: "false"
            ) %>
          </.input_wrapper>
          <.input_wrapper form={f} field={:value}>
            <div class="input-label">Value</div>
            <%= text_input(f, :value,
              value: @data["value"],
              class: "input",
              autofocus: !!@prefill_secret_label || unavailable_secret?(@preselect_name, @secrets),
              spellcheck: "false"
            ) %>
          </.input_wrapper>
          <div class="flex space-x-2">
            <button class="button-base button-blue" type="submit" disabled={f.errors != []}>
              Save
            </button>
            <%= live_patch("Cancel", to: @return_to, class: "button-base button-outlined-gray") %>
          </div>
        </div>
      </.form>
      <%= if @select_secret_ref do %>
        <h3 class="text-2xl font-semibold text-gray-800">
          Select secret
        </h3>
        <.form
          let={_}
          for={:secrets}
          phx-submit="save_secret"
          phx-change="select_secret"
          phx-target={@myself}
        >
          <.select name="secret" selected={@preselect_name} options={secret_options(@secrets)} />
        </.form>
      <% end %>
    </div>
    """
  end

  @impl true
  def handle_event("save", %{"data" => data}, socket) do
    secret_label = String.upcase(data["label"])

    if data_errors(data) == [] do
      secret = %{label: secret_label, value: data["value"]}
      Livebook.Session.put_secret(socket.assigns.session.pid, secret)

      {:noreply,
       socket |> push_patch(to: socket.assigns.return_to) |> push_secret_selected(secret_label)}
    else
      {:noreply, assign(socket, data: data)}
    end
  end

  def handle_event("select_secret", %{"secret" => secret_label}, socket) do
    {:noreply,
     socket |> push_patch(to: socket.assigns.return_to) |> push_secret_selected(secret_label)}
  end

  def handle_event("validate", %{"data" => data}, socket) do
    {:noreply, assign(socket, data: data)}
  end

  defp data_errors(data) do
    Enum.flat_map(data, fn {key, value} ->
      if error = data_error(key, value) do
        [{String.to_existing_atom(key), {error, []}}]
      else
        []
      end
    end)
  end

  defp data_error("label", value) do
    cond do
      String.match?(value, ~r/^\w+$/) -> nil
      value == "" -> "can't be blank"
      true -> "is invalid"
    end
  end

  defp data_error("value", ""), do: "can't be blank"
  defp data_error(_key, _value), do: nil

  defp secret_options(secrets), do: [{"", ""} | Enum.map(secrets, &{&1.label, &1.label})]

  defp push_secret_selected(%{assigns: %{select_secret_ref: nil}} = socket, _), do: socket

  defp push_secret_selected(%{assigns: %{select_secret_ref: ref}} = socket, secret_label) do
    push_event(socket, "secret_selected", %{select_secret_ref: ref, secret_label: secret_label})
  end

  defp prefill_secret_label(socket) do
    case socket.assigns.prefill_secret_label do
      nil ->
        if unavailable_secret?(socket.assigns.preselect_name, socket.assigns.secrets),
          do: socket.assigns.preselect_name,
          else: ""

      prefill ->
        prefill
    end
  end

  defp unavailable_secret?(nil, _), do: false
  defp unavailable_secret?("", _), do: false

  defp unavailable_secret?(preselect_name, secrets) do
    preselect_name not in Enum.map(secrets, & &1.label)
  end
end
