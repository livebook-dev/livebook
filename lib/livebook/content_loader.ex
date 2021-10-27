defmodule Livebook.ContentLoader do
  @moduledoc false

  alias Livebook.Utils.HTTP

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

        %{
          uri
          | path: path,
            host: "raw.githubusercontent.com",
            authority: "raw.githubusercontent.com"
        }

      _ ->
        uri
    end
  end

  defp do_rewrite_url(%URI{host: "gist.github.com"} = uri) do
    case String.split(uri.path, "/") do
      ["", owner, hash] ->
        path = Enum.join(["", owner, hash, "raw"], "/")

        %{
          uri
          | path: path,
            host: "gist.githubusercontent.com",
            authority: "gist.githubusercontent.com"
        }

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
end
