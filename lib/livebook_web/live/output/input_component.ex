defmodule LivebookWeb.Output.InputComponent do
  use LivebookWeb, :live_component

  @impl true
  def mount(socket) do
    {:ok, assign(socket, local: false, counter: 0)}
  end

  @impl true
  def update(assigns, socket) do
    value = assigns.input_values[assigns.attrs.id]

    socket =
      socket
      |> assign(assigns)
      |> assign(value: value)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <form id={"#{@id}-form-#{@counter}"} phx-change="change" phx-submit="submit" phx-target={@myself}>
      <div class="input-label">
        <%= @attrs.label %>
      </div>

      <.input
        id={"#{@id}-input"}
        attrs={@attrs}
        value={@value}
        myself={@myself} />
    </form>
    """
  end

  defp input(%{attrs: %{type: :select}} = assigns) do
    ~H"""
    <select
      data-el-input
      class="input input-select"
      name="value">
      <%= for {{key, label}, idx} <- Enum.with_index(@attrs.options) do %>
        <option value={idx} selected={@value == key}>
          <%= label %>
        </option>
      <% end %>
    </select>
    """
  end

  defp input(%{attrs: %{type: :checkbox}} = assigns) do
    ~H"""
    <div class="mt-1">
      <.switch_checkbox
        data-el-input
        name="value"
        checked={@value} />
    </div>
    """
  end

  defp input(%{attrs: %{type: :range}} = assigns) do
    ~H"""
    <div class="flex items-center space-x-2">
      <div><%= @attrs.min %></div>
      <input type="range"
        data-el-input
        class="input-range"
        name="value"
        value={@value}
        phx-debounce="blur"
        phx-target={@myself}
        spellcheck="false"
        autocomplete="off"
        min={@attrs.min}
        max={@attrs.max}
        step={@attrs.step} />
      <div><%= @attrs.max %></div>
    </div>
    """
  end

  defp input(%{attrs: %{type: :textarea}} = assigns) do
    ~H"""
    <textarea
      data-el-input
      class="input min-h-[200px] tiny-scrollbar"
      name="value"
      phx-debounce="blur"
      phx-target={@myself}
      spellcheck="false"><%= [?\n, @value] %></textarea>
    """
  end

  defp input(%{attrs: %{type: :password}} = assigns) do
    ~H"""
    <.with_password_toggle id={"#{@id}-password-toggle"}>
      <input type="password"
        data-el-input
        class="input w-auto bg-gray-50"
        name="value"
        value={@value}
        phx-debounce="blur"
        phx-target={@myself}
        spellcheck="false"
        autocomplete="off" />
    </.with_password_toggle>
    """
  end

  defp input(%{attrs: %{type: type}} = assigns) when type in [:number, :color, :url, :text] do
    ~H"""
    <input type={html_input_type(@attrs.type)}
      data-el-input
      class="input w-auto invalid:input--error"
      name="value"
      value={to_string(@value)}
      phx-debounce="blur"
      phx-target={@myself}
      spellcheck="false"
      autocomplete="off" />
    """
  end

  defp input(assigns) do
    ~H"""
    <div class="text-red-600">
      Unknown input type <%= @attrs.type %>
    </div>
    """
  end

  defp html_input_type(:number), do: "number"
  defp html_input_type(:color), do: "color"
  defp html_input_type(:url), do: "url"
  defp html_input_type(:text), do: "text"

  @impl true
  def handle_event("change", %{"value" => html_value}, socket) do
    case handle_change(socket, html_value) do
      {:ok, socket} ->
        {:noreply, socket}

      :error ->
        # Force the current value
        {:noreply, update(socket, :counter, &(&1 + 1))}
    end
  end

  def handle_event("submit", %{"value" => html_value}, socket) do
    case handle_change(socket, html_value) do
      {:ok, socket} ->
        send(self(), {:queue_bound_cells_evaluation, socket.assigns.attrs.id})
        {:noreply, socket}

      :error ->
        {:noreply, socket}
    end
  end

  defp handle_change(socket, html_value) do
    case parse(html_value, socket.assigns.attrs) do
      {:ok, value} ->
        prev_value = socket.assigns.value

        socket = assign(socket, value: value)

        if value != prev_value do
          report_change(socket)
        end

        {:ok, socket}

      {:error, _error} ->
        :error
    end
  end

  defp report_change(%{assigns: assigns} = socket) do
    send(self(), {:set_input_values, [{assigns.attrs.id, assigns.value}], assigns.local})

    unless assigns.local do
      report_event(socket, assigns.value)
    end
  end

  defp parse(html_value, %{type: :text}) do
    {:ok, html_value}
  end

  defp parse(html_value, %{type: :textarea}) do
    # The browser may normalize newlines to \r\n, but we prefer just \n
    value = String.replace(html_value, "\r\n", "\n")
    {:ok, value}
  end

  defp parse(html_value, %{type: :password}) do
    {:ok, html_value}
  end

  defp parse(html_value, %{type: :number}) do
    if html_value == "" do
      {:ok, nil}
    else
      case Integer.parse(html_value) do
        {number, ""} ->
          {:ok, number}

        _ ->
          {number, ""} = Float.parse(html_value)
          {:ok, number}
      end
    end
  end

  defp parse(html_value, %{type: :url}) do
    cond do
      html_value == "" -> {:ok, nil}
      Livebook.Utils.valid_url?(html_value) -> {:ok, html_value}
      true -> {:error, "not a valid URL"}
    end
  end

  defp parse(html_value, %{type: :select, options: options}) do
    selected_idx = String.to_integer(html_value)

    options
    |> Enum.with_index()
    |> Enum.find_value(fn {{key, _label}, idx} ->
      idx == selected_idx && {:ok, key}
    end)
  end

  defp parse(html_value, %{type: :checkbox}) do
    {:ok, html_value == "true"}
  end

  defp parse(html_value, %{type: :range}) do
    {number, ""} = Float.parse(html_value)
    {:ok, number}
  end

  defp parse(html_value, %{type: :color}) do
    {:ok, html_value}
  end

  defp report_event(socket, value) do
    topic = socket.assigns.attrs.ref
    event = %{value: value, origin: self(), type: :change}
    send(socket.assigns.attrs.destination, {:event, topic, event})
  end
end
