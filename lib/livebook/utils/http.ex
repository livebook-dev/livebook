defmodule Livebook.Utils.HTTP do
  @moduledoc false

  @type status :: non_neg_integer()
  @type headers :: list(header())
  @type header :: {String.t(), String.t()}

  @doc """
  Retrieves the header value from response headers.
  """
  @spec fetch_header(headers(), String.t()) :: {:ok, String.t()} | :error
  def fetch_header(headers, key) do
    case Enum.find(headers, &match?({^key, _}, &1)) do
      {_, value} -> {:ok, value}
      _ -> :error
    end
  end

  @doc """
  Retrieves content type from response headers.
  """
  @spec fetch_content_type(headers()) :: {:ok, String.t()} | :error
  def fetch_content_type(headers) do
    with {:ok, value} <- fetch_header(headers, "content-type") do
      {:ok, value |> String.split(";") |> hd()}
    end
  end

  @doc """
  Makes an HTTP request.

  ## Options

    * `headers` - request headers, defaults to `[]`

    * `body` - request body given as `{content_type, body}`

    * `timeout` - request timeout, defaults to 10 seconds
  """
  @spec request(atom(), String.t(), keyword()) ::
          {:ok, status(), headers(), binary()} | {:error, term()}
  def request(method, url, opts \\ [])
      when is_atom(method) and is_binary(url) and is_list(opts) do
    headers = build_headers(opts[:headers] || [])

    request =
      case opts[:body] do
        nil -> {url, headers}
        {content_type, body} -> {url, headers, to_charlist(content_type), body}
      end

    http_opts = [
      ssl: http_ssl_opts(),
      timeout: opts[:timeout] || 10_000
    ]

    opts = [
      body_format: :binary
    ]

    case :httpc.request(method, request, http_opts, opts) do
      {:ok, {{_, status, _}, headers, body}} ->
        {:ok, status, parse_headers(headers), body}

      {:error, error} ->
        {:error, error}
    end
  end

  defp build_headers(entries) do
    headers =
      Enum.map(entries, fn {key, value} ->
        {to_charlist(key), to_charlist(value)}
      end)

    [{'user-agent', 'livebook'} | headers]
  end

  defp parse_headers(headers) do
    Enum.map(headers, fn {key, val} ->
      {String.downcase(to_string(key)), to_string(val)}
    end)
  end

  # Load SSL certificates

  crt_file = CAStore.file_path()
  crt = File.read!(crt_file)
  pems = :public_key.pem_decode(crt)
  ders = Enum.map(pems, fn {:Certificate, der, _} -> der end)

  # Note: we need to load the certificates at compilation time,
  # as we don't have access to package files in Escript.
  @cacerts ders

  defp http_ssl_opts() do
    # Use secure options, see https://gist.github.com/jonatanklosko/5e20ca84127f6b31bbe3906498e1a1d7
    [
      verify: :verify_peer,
      cacerts: @cacerts,
      customize_hostname_check: [
        match_fun: :public_key.pkix_verify_hostname_match_fun(:https)
      ]
    ]
  end
end
