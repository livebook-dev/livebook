defmodule Livebook.ContentLoader do
  @moduledoc false

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
    case :httpc.request(:get, {url, []}, http_opts(), body_format: :binary) do
      {:ok, {{_, 200, _}, headers, body}} ->
        valid_content? =
          case fetch_content_type(headers) do
            {:ok, content_type} -> content_type in ["text/plain", "text/markdown"]
            :error -> false
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

  defp fetch_content_type(headers) do
    case Enum.find(headers, fn {key, _} -> key == 'content-type' end) do
      {_, value} ->
        {:ok,
         value
         |> List.to_string()
         |> String.split(";")
         |> hd()}

      _ ->
        :error
    end
  end

  crt_file = CAStore.file_path()
  crt = File.read!(crt_file)
  pems = :public_key.pem_decode(crt)
  ders = Enum.map(pems, fn {:Certificate, der, _} -> der end)

  # Note: we need to load the certificates at compilation time,
  # as we don't have access to package files in Escript.
  @cacerts ders

  defp http_opts() do
    [
      # Use secure options, see https://gist.github.com/jonatanklosko/5e20ca84127f6b31bbe3906498e1a1d7
      ssl: [
        verify: :verify_peer,
        cacerts: @cacerts,
        customize_hostname_check: [
          match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
        ]
      ]
    ]
  end
end
