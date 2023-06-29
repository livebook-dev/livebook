defmodule LivebookWeb.Confirm do
  use Phoenix.Component

  import LivebookWeb.CoreComponents
  import LivebookWeb.FormComponents
  import Phoenix.LiveView

  alias Phoenix.LiveView.JS

  @doc """
  Shows a confirmation modal.

  On confirmation runs `on_confirm`. The function receives a socket
  and should return the socket. Note that this socket always comes from
  the root LV level, keep this in mind when using in a component and
  dealing with assigns.

  Make sure to render `confirm_root/1` in the layout.

  ## Options

    * `:title` - title of the confirmation modal. Defaults to `"Are you sure?"`

    * `:description` - content of the confirmation modal. Required

    * `:confirm_text` - text of the confirm button. Defaults to `"Yes"`

    * `:confirm_icon` - icon in the confirm button. Optional

    * `:danger` - whether the action is destructive or regular. Defaults to `true`

    * `:opt_out_id` - enables the "Don't show this message again"
      checkbox. Once checked by the user, the confirmation with this
      id is never shown again. Optional

    * `:options` - a list of togglable options to present alongside the
      confirmation message. Each option must be a map:

      ```
      %{
        name: String.t(),
        label: String.t(),
        default: boolean(),
        disabled: boolean()
      }
      ```

      The option values are passed to the `on_confirm` function as the
      second argument

  """
  def confirm(socket, on_confirm, opts) do
    opts =
      Keyword.validate!(
        opts,
        title: "Are you sure?",
        description: nil,
        confirm_text: "Yes",
        confirm_icon: nil,
        danger: true,
        opt_out_id: nil,
        options: []
      )

    send(self(), {:confirm, on_confirm, opts})

    socket
  end

  @doc """
  Renders the confirmation modal for `confirm/3`.
  """

  attr :confirm_state, :map, required: true

  def confirm_root(assigns) do
    ~H"""
    <.confirm_modal :if={@confirm_state} id={"confirm-#{@confirm_state.id}"} {@confirm_state.attrs} />
    """
  end

  defp confirm_modal(assigns) do
    ~H"""
    <.modal id={@id} width={:medium} show={true}>
      <form
        id={"#{@id}-confirm-content"}
        class="p-6 flex flex-col"
        phx-submit={JS.push("confirm") |> hide_modal(@id)}
        data-el-confirm-form
      >
        <h3 class="text-2xl font-semibold text-gray-800">
          <%= @title %>
        </h3>
        <p class="mt-8 text-gray-700">
          <%= @description %>
        </p>
        <div :if={@options != []} class="mt-8">
          <h3 class="mb-2 text-lg font-semibold text-gray-800">
            Options
          </h3>
          <div class="flex flex-col gap-2">
            <.switch_field
              :for={option <- @options}
              name={"options[#{option.name}]"}
              label={option.label}
              value={option.default}
              disabled={option.disabled}
            />
          </div>
        </div>
        <label :if={@opt_out_id} class="mt-6 text-gray-700 flex items-center">
          <input class="checkbox mr-3" type="checkbox" name="opt_out_id" value={@opt_out_id} />
          <span class="text-sm">
            Don't show this message again
          </span>
        </label>
        <div class="mt-8 flex justify-end">
          <div class={["flex gap-2", @danger && "flex-row-reverse"]}>
            <button class="button-base button-outlined-gray" type="button" phx-click={hide_modal(@id)}>
              Cancel
            </button>
            <button
              class={["button-base", if(@danger, do: "button-red", else: "button-blue")]}
              type="submit"
            >
              <.remix_icon :if={@confirm_icon} icon={@confirm_icon} class="align-middle mr-1" />
              <span><%= @confirm_text %></span>
            </button>
          </div>
        </div>
      </form>
    </.modal>
    """
  end

  def on_mount(:default, _params, _session, socket) do
    connect_params = get_connect_params(socket) || %{}

    # Opting out is a per-user preference, so we store it on the client
    # and send in the connect params
    confirm_opt_out_ids = connect_params["confirm_opt_out_ids"] || []

    socket =
      socket
      |> assign(confirm_state: nil, confirm_opt_out_ids: MapSet.new(confirm_opt_out_ids))
      |> attach_hook(:confirm, :handle_event, &handle_event/3)
      |> attach_hook(:confirm, :handle_info, &handle_info/2)

    {:cont, socket}
  end

  defp handle_event("confirm", params, socket) do
    socket =
      if opt_out_id = params["opt_out_id"] do
        socket
        |> update(:confirm_opt_out_ids, &MapSet.put(&1, opt_out_id))
        |> push_event("add_confirm_opt_out_id", %{opt_out_id: opt_out_id})
      else
        socket
      end

    options =
      for {name, value} <- params["options"] || [], into: %{}, do: {name, value == "true"}

    on_confirm = socket.assigns.confirm_state.on_confirm

    socket =
      cond do
        is_function(on_confirm, 1) -> on_confirm.(socket)
        is_function(on_confirm, 2) -> on_confirm.(socket, options)
      end

    {:halt, socket}
  end

  defp handle_event(_event, _params, socket), do: {:cont, socket}

  defp handle_info({:confirm, on_confirm, opts}, socket) do
    socket =
      if opts[:opt_out_id] && opts[:opt_out_id] in socket.assigns.confirm_opt_out_ids do
        on_confirm.(socket)
      else
        assign(socket,
          confirm_state: %{
            id: Livebook.Utils.random_short_id(),
            on_confirm: on_confirm,
            attrs: opts
          }
        )
      end

    {:cont, socket}
  end

  defp handle_info(_message, socket), do: {:cont, socket}
end
