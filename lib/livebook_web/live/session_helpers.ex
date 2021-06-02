defmodule LivebookWeb.SessionHelpers do
  import Phoenix.LiveView
  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Creates a new session, redirects on success,
  puts an error flash message on failure.

  Accepts the same options as `Livebook.SessionSupervisor.create_session/1`.
  """
  @spec create_session(Phoenix.LiveView.Socket.t(), keyword()) :: Phoenix.LiveView.Socket.t()
  def create_session(socket, opts \\ []) do
    case Livebook.SessionSupervisor.create_session(opts) do
      {:ok, id} ->
        push_redirect(socket, to: Routes.session_path(socket, :page, id))

      {:error, reason} ->
        put_flash(socket, :error, "Failed to create session: #{reason}")
    end
  end
end
