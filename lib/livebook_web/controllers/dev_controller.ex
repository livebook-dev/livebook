defmodule LivebookWeb.DevController do
  use LivebookWeb, :controller

  alias Livebook.LiveMarkdown

  plug :disallow_browser
  plug :require_enabled

  def sync(conn, %{"file" => path}) when is_binary(path) do
    file = Livebook.FileSystem.File.local(path)

    session =
      Livebook.Sessions.list_sessions()
      |> Enum.find(fn session ->
        session.file != nil and Livebook.FileSystem.File.equal?(session.file, file)
      end)

    if session do
      Livebook.Session.sync_file(session.pid)
      json(conn, %{status: "ok"})
    else
      conn
      |> put_status(404)
      |> json(%{status: "error", message: "No session found for the given file"})
    end
  end

  def open(conn, %{"file" => path}) when is_binary(path) do
    file = Livebook.FileSystem.File.local(path)

    session =
      Livebook.Sessions.list_sessions()
      |> Enum.find(fn session ->
        session.file != nil and Livebook.FileSystem.File.equal?(session.file, file)
      end)

    if session do
      json(conn, %{path: ~p"/sessions/#{session.id}"})
    else
      case Livebook.FileSystem.File.read(file) do
        {:ok, content} ->
          {notebook, _} = LiveMarkdown.notebook_from_livemd(content)

          {:ok, session} =
            Livebook.Sessions.create_session(
              notebook: notebook,
              file: file,
              origin: {:file, file}
            )

          json(conn, %{path: ~p"/sessions/#{session.id}"})

        {:error, reason} ->
          conn
          |> put_status(422)
          |> json(%{status: "error", message: "Failed to read file: #{reason}"})
      end
    end
  end

  def restamp(conn, %{"old_source" => old_source, "new_source" => new_source})
      when is_binary(old_source) and is_binary(new_source) do
    {notebook_before, %{has_stamp?: has_stamp?, stamp_verified?: stamp_verified?}} =
      LiveMarkdown.notebook_from_livemd(old_source)

    if has_stamp? and not stamp_verified? do
      conn
      |> put_status(422)
      |> json(%{status: "error", message: "The old_source stamp is invalid"})
    else
      {notebook_after, %{stamp_verified?: new_stamp_verified?}} =
        LiveMarkdown.notebook_from_livemd(new_source)

      if new_stamp_verified? do
        json(conn, %{source: new_source})
      else
        stamp_metadata = LiveMarkdown.Export.notebook_stamp_metadata(notebook_before)
        notebook_after = LiveMarkdown.Import.apply_stamp_metadata(notebook_after, stamp_metadata)

        {source, _warnings} = LiveMarkdown.notebook_to_livemd(notebook_after)

        json(conn, %{source: source})
      end
    end
  end

  defp disallow_browser(conn, _opts) do
    if get_req_header(conn, "origin") == [] do
      conn
    else
      conn
      |> put_status(403)
      |> json(%{status: "error", message: "This endpoint is not available in the browser"})
      |> halt()
    end
  end

  defp require_enabled(conn, _opts) do
    if Livebook.Settings.dev_endpoints_enabled?() do
      conn
    else
      conn
      |> put_status(403)
      |> json(%{
        status: "error",
        message: "Dev endpoints are disabled, you can enable them in the settings"
      })
      |> halt()
    end
  end
end
