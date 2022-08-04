defmodule LivebookWeb.SessionHelpers do
  import Phoenix.LiveView
  alias LivebookWeb.Router.Helpers, as: Routes

  @doc """
  Creates a new session, redirects on success,
  puts an error flash message on failure.

  Accepts the same options as `Livebook.Sessions.create_session/1`.
  """
  @spec create_session(Phoenix.LiveView.Socket.t(), keyword()) :: Phoenix.LiveView.Socket.t()
  def create_session(socket, opts \\ []) do
    # Revert persistence options to default values if there is
    # no file attached to the new session
    opts =
      if opts[:notebook] != nil and opts[:file] == nil do
        Keyword.update!(opts, :notebook, &Livebook.Notebook.reset_persistence_options/1)
      else
        opts
      end

    case Livebook.Sessions.create_session(opts) do
      {:ok, session} ->
        redirect_path =
          socket
          |> Routes.session_path(:page, session.id)
          |> maybe_add_anchor_link(opts)

        push_redirect(socket, to: redirect_path)

      {:error, reason} ->
        put_flash(socket, :error, "Failed to create session: #{reason}")
    end
  end

  defp maybe_add_anchor_link(redirect_path, opts) do
    case opts[:anchor_link] do
      nil -> redirect_path
      anchor_link -> "#{redirect_path}##{anchor_link}"
    end
  end

  @doc """
  Formats the given list of notebook import messages and puts
  into the warning flash.
  """
  @spec put_import_warnings(Phoenix.LiveView.Socket.t(), list(String.t())) ::
          Phoenix.LiveView.Socket.t()
  def put_import_warnings(socket, messages)

  def put_import_warnings(socket, []), do: socket

  def put_import_warnings(socket, messages) do
    list =
      messages
      |> Enum.map(fn message -> ["- ", message] end)
      |> Enum.intersperse("\n")

    flash =
      IO.iodata_to_binary([
        "We found problems while importing the file and tried to autofix them:\n" | list
      ])

    put_flash(socket, :warning, flash)
  end

  def uses_memory?(%{runtime: %{total: total}}) when total > 0, do: true
  def uses_memory?(_), do: false
end
