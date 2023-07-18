defmodule LivebookWeb.OpenLive.SourceComponent do
  use LivebookWeb, :live_component

  import Ecto.Changeset

  @impl true
  def mount(socket) do
    {:ok, assign(socket, changeset: changeset())}
  end

  defp changeset(attrs \\ %{}) do
    data = %{source: nil}
    types = %{source: :string}

    cast({data, types}, attrs, [:source])
    |> validate_required([:source])
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
        for={@changeset}
        as={:data}
        id="import-source"
        phx-submit="import"
        phx-change="validate"
        phx-target={@myself}
        autocomplete="off"
      >
        <.textarea_field
          field={f[:source]}
          label="Notebook source"
          resizable={false}
          autofocus
          aria-labelledby="import-from-source"
          spellcheck="false"
          rows="5"
        />
        <button class="mt-5 button-base button-blue" type="submit" disabled={not @changeset.valid?}>
          Import
        </button>
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
    data
    |> changeset()
    |> apply_action(:insert)
    |> case do
      {:ok, data} ->
        send(self(), {:import_source, data.source, []})
        {:noreply, socket}

      {:error, changeset} ->
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
end
