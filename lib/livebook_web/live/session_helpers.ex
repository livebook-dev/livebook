defmodule LivebookWeb.SessionHelpers do
  import Phoenix.LiveView

  use LivebookWeb, :html

  alias Phoenix.LiveView.Socket
  alias Livebook.Session
  alias Livebook.FileSystem

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
      {:ok, {notebook, messages}} ->
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

  def cell_icon(%{cell_type: :code, language: :elixir} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 bg-purple-100 rounded items-center justify-center">
      <svg width="11" height="15" viewBox="0 0 11 15" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path
          d="M5.7784 3.58083C7.4569 5.87527 9.67878 5.70652 10.0618 9.04833C10.1147 12.9425 8.03684
        14.27 6.55353 14.6441C4.02227 15.3635 1.7644 14.2813 0.875648 11.8316C-0.83154 7.89408 2.36684
        1.41746 4.42502 0.0668945C4.60193 1.32119 5.05745 2.51995 5.75815 3.57521L5.7784 3.58083Z"
          fill="#663299"
        />
      </svg>
    </div>
    """
  end

  def cell_icon(%{cell_type: :code, language: :erlang} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 bg-red-100 rounded items-center justify-center">
      <svg width="18" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 15 10">
        <g fill="#a90533">
          <path d="M2.4 10A7.7 7.7 0 0 1 .5 4.8c0-2 .6-3.6 1.6-4.8H0v10ZM13 10c.5-.6 1-1.2 1.4-2l-2.3-1.2c-.8 1.4-2 2.6-3.6 2.6-2.3 0-3.2-2-3.2-4.8H14V4c0-1.6-.3-3-1-4H15v10h-2Zm0 0" />
          <path d="M5.5 2.3c.1-1.2 1-2 2.1-2s1.9.8 2 2Zm0 0" />
        </g>
      </svg>
    </div>
    """
  end

  def cell_icon(%{cell_type: :markdown} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 bg-blue-100 rounded items-center justify-center">
      <svg width="16" height="14" viewBox="0 0 16 14" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path
          d="M1.25 0.25H14.75C14.9489 0.25 15.1397 0.329018 15.2803 0.46967C15.421 0.610322 15.5 0.801088
        15.5 1V13C15.5 13.1989 15.421 13.3897 15.2803 13.5303C15.1397 13.671 14.9489 13.75 14.75 13.75H1.25C1.05109
        13.75 0.860322 13.671 0.71967 13.5303C0.579018 13.3897 0.5 13.1989 0.5 13V1C0.5 0.801088 0.579018 0.610322
        0.71967 0.46967C0.860322 0.329018 1.05109 0.25 1.25 0.25ZM4.25 9.625V6.625L5.75 8.125L7.25
        6.625V9.625H8.75V4.375H7.25L5.75 5.875L4.25 4.375H2.75V9.625H4.25ZM12.5 7.375V4.375H11V7.375H9.5L11.75
        9.625L14 7.375H12.5Z"
          fill="#3E64FF"
        />
      </svg>
    </div>
    """
  end

  def cell_icon(%{cell_type: :smart} = assigns) do
    ~H"""
    <div class="flex w-6 h-6 bg-red-100 rounded items-center justify-center">
      <.remix_icon icon="flashlight-line text-red-900" />
    </div>
    """
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
        push_redirect(socket, to: redirect_to)
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
end
