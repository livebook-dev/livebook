defmodule LiveBookWeb.Helpers do
  import Phoenix.LiveView.Helpers

  @doc """
  Renders a component inside the `LiveBook.ModalComponent` component.

  The rendered modal receives a `:return_to` option to properly update
  the URL when the modal is closed.
  """
  def live_modal(socket, component, opts) do
    path = Keyword.fetch!(opts, :return_to)
    modal_opts = [id: :modal, return_to: path, component: component, opts: opts]
    live_component(socket, LiveBookWeb.ModalComponent, modal_opts)
  end

  @doc """
  Determines if the request comes from a Mac user based on the *User-Agent* header.
  """
  def mac_user?(conn) do
    [user_agent] = Plug.Conn.get_req_header(conn, "user-agent")
    String.match?(user_agent, ~r/Mac OS X/)
  end
end
