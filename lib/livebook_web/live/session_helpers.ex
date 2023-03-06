defmodule LivebookWeb.SessionHelpers do
  import Phoenix.LiveView

  use LivebookWeb, :verified_routes

  alias Phoenix.LiveView.Socket
  alias Livebook.Session
  alias Livebook.FileSystem

  @doc """
  Creates a new session, redirects on success,
  puts an error flash message on failure.

  Accepts the same options as `Livebook.Sessions.create_session/1`.
  """
  @spec create_session(Socket.t(), keyword()) :: Socket.t()
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
        redirect_path = session_path(session.id, opts)
        push_navigate(socket, to: redirect_path)

      {:error, reason} ->
        put_flash(socket, :error, "Failed to create session: #{reason}")
    end
  end

  @doc """
  Generate the session path based on the provided options.
  """
  @spec session_path(Session.id(), keyword()) :: String.t()
  def session_path(session_id, opts \\ []) do
    maybe_add_url_hash(~p"/sessions/#{session_id}", opts)
  end

  defp maybe_add_url_hash(redirect_path, opts) do
    case opts[:url_hash] do
      nil -> redirect_path
      url_hash -> "#{redirect_path}##{url_hash}"
    end
  end

  @doc """
  Formats the given list of notebook import messages and puts
  into the warning flash.
  """
  @spec put_import_warnings(Socket.t(), list(String.t())) :: Socket.t()
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

  @doc """
  Updates a list of sessions based on the given `Sessions` event.
  """
  @spec update_session_list(
          list(Session.t()),
          {:session_created | :session_updated | :session_closed, Session.t()}
        ) :: list(Session.t())
  def update_session_list(sessions, {:session_created, session}) do
    if session in sessions do
      sessions
    else
      [session | sessions]
    end
  end

  def update_session_list(sessions, {:session_updated, session}) do
    Enum.map(sessions, fn other ->
      if other.id == session.id, do: session, else: other
    end)
  end

  def update_session_list(sessions, {:session_closed, session}) do
    Enum.reject(sessions, &(&1.id == session.id))
  end

  @doc """
  Creates a new session by forking the given notebook file.
  """
  @spec fork_notebook(Socket.t(), FileSystem.File.t()) :: Socket.t()
  def fork_notebook(socket, file) do
    case import_notebook(file) do
      {:ok, {notebook, messages}} ->
        notebook = Livebook.Notebook.forked(notebook)
        images_dir = Session.images_dir_for_notebook(file)

        socket
        |> put_import_warnings(messages)
        |> create_session(
          notebook: notebook,
          copy_images_from: images_dir,
          origin: {:file, file}
        )

      {:error, error} ->
        put_flash(socket, :error, Livebook.Utils.upcase_first(error))
    end
  end

  @doc """
  Creates a new session by opening the given notebook file.
  """
  @spec open_notebook(Socket.t(), FileSystem.File.t()) :: Socket.t()
  def open_notebook(socket, file) do
    case import_notebook(file) do
      {:ok, {notebook, messages}} ->
        socket
        |> put_import_warnings(messages)
        |> create_session(notebook: notebook, file: file, origin: {:file, file})

      {:error, error} ->
        put_flash(socket, :error, Livebook.Utils.upcase_first(error))
    end
  end

  defp import_notebook(file) do
    with {:ok, content} <- FileSystem.File.read(file) do
      {:ok, Livebook.LiveMarkdown.notebook_from_livemd(content)}
    end
  end
end
