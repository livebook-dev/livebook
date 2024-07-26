defmodule Livebook.FileSystem.S3.Client do
  alias Livebook.FileSystem
  alias Livebook.FileSystem.S3

  @doc """
  Sends a request to the bucket to get list of objects.
  """
  @spec list_objects(S3.t(), keyword()) :: {:ok, map()} | {:error, String.t()}
  def list_objects(file_system, opts) do
    prefix = opts[:prefix]
    delimiter = opts[:delimiter]
    query = %{"list-type" => "2", "prefix" => prefix, "delimiter" => delimiter}

    case get(file_system, "/", query: query) do
      {:ok, 200, _headers, %{"ListBucketResult" => result}} ->
        file_keys = xml_get_list(result, "Contents", "Key")
        prefix_keys = xml_get_list(result, "CommonPrefixes", "Prefix")

        {:ok, %{bucket: result["Name"], keys: file_keys ++ prefix_keys}}

      other ->
        request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to get the bucket name.
  """
  @spec get_bucket_name(S3.t()) :: {:ok, String.t()} | {:error, String.t()}
  def get_bucket_name(file_system) do
    # We have the bucket URL, but it's not straightforward to extract
    # the bucket name from the URL, because it may be either the path
    # or a part of the host.
    #
    # Endpoints that return bucket information doesn't include the
    # name, but the listing endpoint does, so we just list keys
    # with an upper limit of 0 and retrieve the bucket name.

    case get(file_system, "/", query: %{"list-type" => "2", "max-keys" => "0"}) do
      {:ok, 200, _headers, %{"ListBucketResult" => result}} -> {:ok, result["Name"]}
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to get an object.
  """
  @spec get_object(S3.t(), String.t()) :: {:ok, map()} | {:error, String.t()}
  def get_object(file_system, key) do
    case get(file_system, "/" <> encode_key(key), long: true, decode: false) do
      {:ok, 200, _headers, body} -> {:ok, body}
      {:ok, 404, _headers, _body} -> FileSystem.Utils.posix_error(:enoent)
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to download an object with multipart.
  """
  @spec multipart_get_object(S3.t(), String.t(), Collectable.t()) ::
          {:ok, map()} | {:error, String.t()}
  def multipart_get_object(file_system, key, collectable) do
    case download(file_system, "/" <> encode_key(key), collectable) do
      {:ok, collectable} -> {:ok, collectable}
      {:error, _message, 404} -> FileSystem.Utils.posix_error(:enoent)
      {:error, message, _status} -> {:error, message}
    end
  end

  @doc """
  Sends a request to the bucket to update an object's content.
  """
  @spec put_object(S3.t(), String.t(), String.t() | nil) :: :ok | {:error, String.t()}
  def put_object(file_system, key, content) do
    case put(file_system, "/" <> encode_key(key), body: content, long: true) do
      {:ok, 200, _headers, _body} -> :ok
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to get the object's ETAG from given key.
  """
  @spec head_object(S3.t(), String.t()) :: {:ok, map()} | {:error, String.t()}
  def head_object(file_system, key) do
    with {:ok, 200, headers, _body} <- head(file_system, "/" <> encode_key(key)),
         {:ok, etag} <- Livebook.Utils.HTTP.fetch_header(headers, "etag") do
      {:ok, %{etag: etag}}
    else
      {:ok, 404, _headers, _body} -> FileSystem.Utils.posix_error(:enoent)
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to copy an object to given destination.
  """
  @spec copy_object(S3.t(), String.t(), String.t(), String.t()) :: :ok | {:error, String.t()}
  def copy_object(file_system, bucket, source_key, destination_key) do
    copy_source = bucket <> "/" <> encode_key(source_key)
    headers = [{"x-amz-copy-source", copy_source}]

    case put(file_system, "/" <> encode_key(destination_key), headers: headers) do
      {:ok, 200, _headers, _body} -> :ok
      {:ok, 404, _headers, _body} -> FileSystem.Utils.posix_error(:enoent)
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to delete an object.
  """
  @spec delete_object(S3.t(), String.t()) :: :ok | {:error, String.t()}
  def delete_object(file_system, key) do
    case delete(file_system, "/" <> encode_key(key)) do
      {:ok, 204, _headers, _body} -> :ok
      {:ok, 404, _headers, _body} -> :ok
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to delete a list of objects.
  """
  @spec delete_objects(S3.t(), list(String.t())) :: :ok | {:error, String.t()}
  def delete_objects(file_system, keys) do
    objects = Enum.map(keys, &%{"Key" => &1})

    body =
      %{"Delete" => %{"Object" => objects, "Quiet" => "true"}}
      |> S3.XML.encode_to_iodata!()
      |> IO.iodata_to_binary()

    body_md5 = :crypto.hash(:md5, body)
    headers = [{"Content-MD5", Base.encode64(body_md5)}]

    case post(file_system, "/", query: %{"delete" => ""}, headers: headers, body: body) do
      {:ok, 200, _headers, %{"Error" => _}} = result -> request_response_to_error(result)
      {:ok, 200, _headers, _body} -> :ok
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to check if object exists.
  """
  @spec object_exists(S3.t(), String.t()) :: {:ok, boolean()} | {:error, String.t()}
  def object_exists(file_system, key) do
    # It is possible for /dir/obj to exist without the /dir/ object,
    # but we still consider it as existing. That's why we list
    # objects instead of checking the key directly.

    with {:ok, %{keys: keys}} <- list_objects(file_system, prefix: key, delimiter: "/") do
      exists? =
        if String.ends_with?(key, "/"),
          do: keys != [],
          else: key in keys

      {:ok, exists?}
    end
  end

  @doc """
  Sends a request to the bucket to multipart upload an object.
  """
  @spec create_multipart_upload(S3.t(), String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def create_multipart_upload(file_system, key) do
    query = %{"uploads" => ""}

    case post(file_system, "/" <> encode_key(key), query: query, body: "") do
      {:ok, 200, _headers, %{"InitiateMultipartUploadResult" => result}} ->
        {:ok, result["UploadId"]}

      other ->
        request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to upload an object partially.
  """
  @spec upload_part(S3.t(), String.t(), String.t(), pos_integer(), String.t()) ::
          {:ok, map()} | {:error, String.t()}
  def upload_part(file_system, key, upload_id, part_number, content) do
    query = %{"uploadId" => upload_id, "partNumber" => part_number}
    opts = [query: query, body: content, long: true]

    with {:ok, 200, headers, _body} <- put(file_system, "/" <> encode_key(key), opts),
         {:ok, etag} <- Livebook.Utils.HTTP.fetch_header(headers, "etag") do
      {:ok, %{etag: etag}}
    else
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to finish the multipart object upload.
  """
  @spec complete_multipart_upload(S3.t(), String.t(), String.t(), list(String.t())) ::
          :ok | {:error, String.t()}
  def complete_multipart_upload(file_system, key, upload_id, etags) do
    query = %{"uploadId" => upload_id}
    parts = for {etag, n} <- Enum.with_index(etags, 1), do: %{"PartNumber" => n, "ETag" => etag}

    body =
      %{"CompleteMultipartUpload" => %{"Part" => parts}}
      |> S3.XML.encode_to_iodata!()
      |> IO.iodata_to_binary()

    case post(file_system, "/" <> encode_key(key), query: query, body: body) do
      {:ok, 200, _headers, _body} -> :ok
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to abort the multipart object upload.
  """
  @spec abort_multipart_upload(S3.t(), String.t(), String.t()) :: :ok | {:error, String.t()}
  def abort_multipart_upload(file_system, key, upload_id) do
    query = %{"uploadId" => upload_id}

    case delete(file_system, "/" <> encode_key(key), query: query) do
      {:ok, 204, _headers, _body} -> :ok
      other -> request_response_to_error(other)
    end
  end

  # Convenient API

  defp get(file_system, path, opts), do: request(file_system, :get, path, opts)
  defp post(file_system, path, opts), do: request(file_system, :post, path, opts)
  defp put(file_system, path, opts), do: request(file_system, :put, path, opts)
  defp head(file_system, path), do: request(file_system, :head, path, [])
  defp delete(file_system, path, opts \\ []), do: request(file_system, :delete, path, opts)

  defp download(file_system, path, collectable, opts \\ []) do
    query = opts[:query] || %{}
    headers = opts[:headers] || []
    url = build_url(file_system, path, query)
    headers = sign_headers(file_system, :get, url, headers)

    Livebook.Utils.HTTP.download(url, collectable, headers: headers)
  end

  # Private

  defp encode_key(key) do
    key
    |> String.split("/")
    |> Enum.map_join("/", fn segment -> URI.encode(segment, &URI.char_unreserved?/1) end)
  end

  defp build_url(file_system, path, query) do
    query_string = URI.encode_query(query, :rfc3986)
    query_string = if query_string != "", do: "?#{query_string}", else: ""

    file_system.bucket_url <> path <> query_string
  end

  defp sign_headers(file_system, method, url, headers, body \\ nil) do
    credentials = S3.credentials(file_system)
    now = NaiveDateTime.utc_now() |> NaiveDateTime.to_erl()
    %{host: host} = URI.parse(file_system.bucket_url)
    headers = [{"Host", host} | headers]

    headers =
      if credentials.token,
        do: [{"X-Amz-Security-Token", credentials.token} | headers],
        else: headers

    :aws_signature.sign_v4(
      credentials.access_key_id,
      credentials.secret_access_key,
      file_system.region,
      "s3",
      now,
      Atom.to_string(method),
      url,
      headers,
      body || "",
      uri_encode_path: false,
      session_token: credentials.token
    )
  end

  defp request(file_system, method, path, opts) do
    long = Keyword.get(opts, :long, false)
    decode? = Keyword.get(opts, :decode, true)

    query = opts[:query] || %{}
    headers = opts[:headers] || []
    body = opts[:body]
    timeout = if long, do: 60_000, else: 30_000

    url = build_url(file_system, path, query)
    headers = sign_headers(file_system, method, url, headers, body)
    body = body && {"application/octet-stream", body}

    result =
      Livebook.Utils.HTTP.request(method, url, headers: headers, body: body, timeout: timeout)

    if decode?, do: decode(result), else: result
  end

  defp decode({:ok, status, headers, body}) do
    if xml?(headers, body),
      do: {:ok, status, headers, S3.XML.decode!(body)},
      else: {:ok, status, headers, body}
  end

  defp decode({:error, _} = error), do: error

  defp xml?(headers, body) do
    guess_xml? = String.starts_with?(body, "<?xml")

    case Livebook.Utils.HTTP.fetch_content_type(headers) do
      {:ok, content_type} when content_type in ["text/xml", "application/xml"] -> true
      # Apparently some requests return XML without content-type
      :error when guess_xml? -> true
      _otherwise -> false
    end
  end

  defp request_response_to_error({:ok, 403, _headers, %{"Error" => %{"Message" => message}}}) do
    {:error, "access denied, " <> Livebook.Utils.downcase_first(message)}
  end

  defp request_response_to_error({:ok, 403, _headers, _body}) do
    {:error, "access denied"}
  end

  defp request_response_to_error({:ok, _status, _headers, %{"Error" => %{"Message" => message}}}) do
    {:error, Livebook.Utils.downcase_first(message)}
  end

  defp request_response_to_error({:ok, _status, _headers, %{"Error" => [_ | _] = errors}}) do
    [%{"Message" => message} | errors] = errors
    {:error, Livebook.Utils.downcase_first(message) <> ", and #{length(errors)} more errors"}
  end

  defp request_response_to_error({:ok, _status, _headers, _body}) do
    {:error, "unexpected response"}
  end

  defp request_response_to_error(_otherwise) do
    {:error, "failed to make an http request"}
  end

  defp xml_get_list(result, xml_key, map_key) do
    items =
      case result do
        %{^xml_key => item} when is_map(item) -> [item]
        %{^xml_key => items} when is_list(items) -> items
        _ -> []
      end

    Enum.map(items, & &1[map_key])
  end
end
