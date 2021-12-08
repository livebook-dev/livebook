defmodule Livebook.ContentLoader do
  @moduledoc false

  alias Livebook.Utils.HTTP

  @typedoc """
  A location from where content gets loaded.
  """
  @type location :: {:file, FileSystem.File.t()} | {:url, String.t()}

  @doc """
  Rewrite known URLs, so that they point to plain text file rather than HTML.

  Currently the rewerites handle:

    * GitHub files
    * Gist files
  """
  @spec rewrite_url(String.t()) :: String.t()
  def rewrite_url(url) do
    url
    |> URI.parse()
    |> do_rewrite_url()
    |> URI.to_string()
  end

  defp do_rewrite_url(%URI{host: "github.com"} = uri) do
    case String.split(uri.path, "/") do
      ["", owner, repo, "blob", hash | file_path] ->
        path = Enum.join(["", owner, repo, hash | file_path], "/")
        %{uri | path: path, host: "raw.githubusercontent.com"}

      _ ->
        uri
    end
  end

  defp do_rewrite_url(%URI{host: "gist.github.com"} = uri) do
    case String.split(uri.path, "/") do
      ["", owner, hash] ->
        path = Enum.join(["", owner, hash, "raw"], "/")
        %{uri | path: path, host: "gist.githubusercontent.com"}

      _ ->
        uri
    end
  end

  defp do_rewrite_url(uri), do: uri

  @doc """
  Loads binary content from the given URl and validates if its plain text.
  """
  @spec fetch_content(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def fetch_content(url) do
    case HTTP.request(:get, url) do
      {:ok, 200, headers, body} ->
        valid_content? =
          case HTTP.fetch_content_type(headers) do
            {:ok, content_type} ->
              content_type in ["text/plain", "text/markdown", "application/octet-stream"]

            :error ->
              false
          end

        if valid_content? do
          {:ok, body}
        else
          {:error, "invalid content type, make sure the URL points to live markdown"}
        end

      _ ->
        {:error, "failed to download notebook from the given URL"}
    end
  end

  @doc """
  Loads a notebook content from the given location.
  """
  @spec fetch_content_from_location(location()) :: {:ok, String.t()} | {:error, String.t()}
  def fetch_content_from_location(location)

  def fetch_content_from_location({:file, file}) do
    case Livebook.FileSystem.File.read(file) do
      {:ok, content} -> {:ok, content}
      {:error, message} -> {:error, "failed to read #{file.path}, reason: #{message}"}
    end
  end

  def fetch_content_from_location({:url, url}) do
    url
    |> rewrite_url()
    |> fetch_content()
  end

  @doc """
  Normalizes the given URL into a location.
  """
  @spec url_to_location(String.t()) :: location()
  def url_to_location(url)

  def url_to_location("file://" <> path) do
    path = Path.expand(path)
    file = Livebook.FileSystem.File.local(path)
    {:file, file}
  end

  def url_to_location(url), do: {:url, url}

  @doc """
  Resolves the given relative path with regard to the given location.
  """
  @spec resolve_location(location(), String.t()) :: location()
  def resolve_location(location, relative_path)

  def resolve_location({:url, url}, relative_path) do
    {:url, Livebook.Utils.expand_url(url, relative_path)}
  end

  def resolve_location({:file, file}, relative_path) do
    {:file, Livebook.FileSystem.File.resolve(file, relative_path)}
  end
end
