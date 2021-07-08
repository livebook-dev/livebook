defmodule LivebookWeb.SessionLive.InputCellSettingsComponent do
  use LivebookWeb, :live_component

  alias Livebook.Session
  alias Livebook.Notebook.Cell

  @impl true
  def update(assigns, socket) do
    cell = assigns.cell

    socket =
      socket
      |> assign(assigns)
      |> assign(:current_type, cell.type)
      |> assign_new(:attrs, fn ->
        Map.take(cell, [:name, :type, :reactive, :props])
      end)
      |> assign_new(:valid, fn -> true end)

    {:ok, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="p-6 pb-4 flex flex-col space-y-8">
      <h3 class="text-2xl font-semibold text-gray-800">
        Cell settings
      </h3>
      <form
        phx-submit="save"
        phx-change="validate"
        phx-target={@myself}
        spellcheck="false"
        autocomplete="off">
        <div class="flex flex-col space-y-6">
          <div>
            <div class="input-label">Type</div>
            <.select name="attrs[type]" selected={@attrs.type} options={input_types()} />
          </div>
          <div>
            <div class="input-label">Name</div>
            <input type="text" class="input" name="attrs[name]" value={@attrs.name} autofocus />
          </div>
          <.extra_fields type={@attrs.type} props={@attrs.props} />
          <.switch_checkbox
            name="attrs[reactive]"
            label="Reactive (reevaluates dependent cells on change)"
            checked={@attrs.reactive} />
        </div>
        <div class="mt-8 flex justify-end space-x-2">
          <%= live_patch "Cancel", to: @return_to, class: "button button-outlined-gray" %>
          <button class="button button-blue" type="submit" disabled={not @valid}>
            Save
          </button>
        </div>
      </form>
    </div>
    """
  end

  defp extra_fields(%{type: :range} = assigns) do
    ~H"""
    <div class="flex space-x-4">
      <div class="flex-grow">
        <div class="input-label">Min</div>
        <input type="number" class="input" name="attrs[props][min]" value={@props.min} />
      </div>
      <div class="flex-grow">
        <div class="input-label">Max</div>
        <input type="number" class="input" name="attrs[props][max]" value={@props.max} />
      </div>
      <div class="flex-grow">
        <div class="input-label">Step</div>
        <input type="number" class="input" name="attrs[props][step]" value={@props.step} />
      </div>
    </div>
    """
  end

  defp extra_fields(assigns), do: ~H""

  @impl true
  def handle_event("validate", params, socket) do
    {valid?, attrs} = validate_attrs(params["attrs"], socket.assigns.attrs)
    {:noreply, socket |> assign(attrs: attrs) |> assign(:valid, valid?)}
  end

  def handle_event("save", params, socket) do
    {true, attrs} = validate_attrs(params["attrs"], socket.assigns.attrs)

    attrs =
      if attrs.type != socket.assigns.current_type do
        Map.put(attrs, :value, default_value(attrs.type, attrs.props))
      else
        attrs
      end

    Session.set_cell_attributes(socket.assigns.session_id, socket.assigns.cell.id, attrs)
    {:noreply, push_patch(socket, to: socket.assigns.return_to)}
  end

  defp validate_attrs(data, prev_attrs) do
    name = data["name"]
    type = data["type"] |> String.to_existing_atom()
    reactive = Map.has_key?(data, "reactive")

    {props_valid?, props} =
      if type == prev_attrs.type do
        data |> Map.get("props", %{}) |> validate_props(type)
      else
        {true, Cell.Input.default_props(type)}
      end

    valid? = name != "" and props_valid?

    {valid?, %{name: name, type: type, reactive: reactive, props: props}}
  end

  defp validate_props(data, :range) do
    min = parse_number(data["min"])
    max = parse_number(data["max"])
    step = parse_number(data["step"])
    valid? = min != nil and max != nil and step != nil and min < max and step > 0
    data = %{min: min, max: max, step: step}
    {valid?, data}
  end

  defp validate_props(_data, _type) do
    {true, %{}}
  end

  defp parse_number(string) do
    case Float.parse(string) do
      {number, _} ->
        integer = round(number)
        if integer == number, do: integer, else: number

      :error ->
        nil
    end
  end

  defp default_value(:color, _props), do: "#3E64FF"
  defp default_value(:range, %{min: min}), do: to_string(min)
  defp default_value(_type, _props), do: ""

  defp input_types do
    [
      color: "Color",
      number: "Number",
      password: "Password",
      text: "Text",
      textarea: "Textarea",
      url: "URL",
      range: "Range"
    ]
  end
end
