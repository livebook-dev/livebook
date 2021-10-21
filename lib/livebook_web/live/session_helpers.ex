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
        push_redirect(socket, to: Routes.session_path(socket, :page, session.id))

      {:error, reason} ->
        put_flash(socket, :error, "Failed to create session: #{reason}")
    end
  end

  @doc """
  Formats the given list of notebook import messages and puts
  into the warning flash.
  """
  @spec put_import_flash_messages(Phoenix.LiveView.Socket.t(), list(String.t())) ::
          Phoenix.LiveView.Socket.t()
  def put_import_flash_messages(socket, messages)

  def put_import_flash_messages(socket, []), do: socket

  def put_import_flash_messages(socket, messages) do
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

  @doc """
  Puts a new info flash for the recently imported notebook
  with the following message:

  "You have imported a notebook, but no code has been evaluated yet.
  You can read the notebook and evaluate code as needed."
  """
  @spec put_import_info_flash(Phoenix.LiveView.Socket.t()) :: Phoenix.LiveView.Socket.t()
  def put_import_info_flash(socket)

  def put_import_info_flash(socket),
    do:
      put_flash(
        socket,
        :info,
        "You have imported a notebook, no code has been executed so far. You should read and evaluate code as needed."
      )
end
