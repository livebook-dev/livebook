defmodule LivebookWeb.SessionHelpers do
  import Phoenix.LiveView

  use LivebookWeb, :html

  alias Phoenix.LiveView.Socket
  alias Livebook.Session
  alias Livebook.FileSystem

  @doc """
  Determines user platform based on the given *User-Agent* header.
  """
  @spec platform_from_user_agent(String.t()) :: :linux | :mac | :windows | :other
  def platform_from_user_agent(user_agent) when is_binary(user_agent) do
    cond do
      linux?(user_agent) -> :linux
      mac?(user_agent) -> :mac
      windows?(user_agent) -> :windows
      true -> :other
    end
  end

  defp linux?(user_agent), do: String.match?(user_agent, ~r/Linux/)
  defp mac?(user_agent), do: String.match?(user_agent, ~r/Mac OS X/)
  defp windows?(user_agent), do: String.match?(user_agent, ~r/Windows/)

  @doc """
  Creates a new session, redirects on success,
  puts an error flash message on failure.

  ## Options

    * `:queue_setup` - whether to queue the setup cell right after
      the session is started. Defaults to `false`

  Accepts the same options as `Livebook.Sessions.create_session/1`.
  """
  @spec create_session(Socket.t(), keyword()) :: Socket.t()
  def create_session(socket, opts \\ []) do
    {queue_setup, opts} = Keyword.pop(opts, :queue_setup, false)

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
        if queue_setup do
          Session.queue_cell_evaluation(session.pid, Livebook.Notebook.Cell.setup_cell_id())
        end

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
      IO.iodata_to_binary(["We found problems while importing the file:\n" | list])

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
      {:ok, {notebook, %{warnings: messages}}} ->
        notebook = Livebook.Notebook.forked(notebook)
        files_dir = Session.files_dir_for_notebook(file)

        socket
        |> put_import_warnings(messages)
        |> create_session(
          notebook: notebook,
          files_source: {:dir, files_dir},
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
      {:ok, {notebook, %{warnings: messages}}} ->
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

  @doc """
  Shows a confirmation modal to delete the given session.

  ## Options

    * `:redirect_to` - a URL to redirect to after closing the session

  """
  def confirm_close_session(socket, session, opts \\ []) do
    redirect_to = opts[:redirect_to]

    on_confirm = fn socket ->
      Livebook.Session.close(session.pid)

      if redirect_to do
        push_navigate(socket, to: redirect_to)
      else
        socket
      end
    end

    assigns = %{notebook_name: session.notebook_name, file: session.file}

    description = ~H"""
    Are you sure you want to close this session - <span class="font-semibold">“<%= @notebook_name %>”</span>?
    <br />
    <%= if @file do %>
      This won't delete any persisted files.
    <% else %>
      The notebook is not persisted and content may be lost.
    <% end %>
    """

    confirm(socket, on_confirm,
      title: "Close session",
      description: description,
      confirm_text: "Close session",
      confirm_icon: "close-circle-line"
    )
  end

  @doc """
  Converts the given arbitrary name to a file entry name.

  The returned name is either valid or empty.
  """
  @spec sanitize_file_entry_name(String.t()) :: String.t() | nil
  def sanitize_file_entry_name(client_name) do
    client_name
    |> String.replace(~r/[^\s\w-.]/u, "")
    |> String.trim()
    |> String.replace(~r/\s+/u, "_")
    |> case do
      "" ->
        ""

      name ->
        if String.contains?(name, ".") do
          name
        else
          name <> ".bin"
        end
    end
  end

  @doc """
  Generates a token for the given input.
  """
  @spec generate_input_token(pid(), String.t()) :: String.t()
  def generate_input_token(live_view_pid, input_id) do
    Phoenix.Token.sign(LivebookWeb.Endpoint, "session-input", %{
      live_view_pid: live_view_pid,
      input_id: input_id
    })
  end

  @doc """
  Verifies token from `generate_input_token/2` and extracts the encoded
  data.
  """
  @spec verify_input_token!(String.t()) :: {pid(), String.t()}
  def verify_input_token!(token) do
    {:ok, %{live_view_pid: live_view_pid, input_id: input_id}} =
      Phoenix.Token.verify(LivebookWeb.Endpoint, "session-input", token)

    {live_view_pid, input_id}
  end

  @doc """
  Registers an uploaded input file in session.
  """
  @spec register_input_file(pid(), String.t(), String.t(), boolean(), String.t()) ::
          {:ok, Livebook.Runtime.file_ref()}
  def register_input_file(session_pid, path, input_id, local, client_id) do
    if local do
      key = "#{input_id}-#{client_id}"
      Livebook.Session.register_file(session_pid, path, key, linked_client_id: client_id)
    else
      key = "#{input_id}-global"
      Livebook.Session.register_file(session_pid, path, key)
    end
  end
end
