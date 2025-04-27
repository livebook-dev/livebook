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
    params = %{"list-type" => "2", "prefix" => prefix, "delimiter" => delimiter}

    case request(file_system, "/", params: params) do
      {:ok, %{status: 200, body: %{"ListBucketResult" => result}}} ->
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

    case request(file_system, "/", params: %{"list-type" => "2", "max-keys" => "0"}) do
      {:ok, %{status: 200, body: %{"ListBucketResult" => result}}} -> {:ok, result["Name"]}
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to get an object.
  """
  @spec get_object(S3.t(), String.t()) :: {:ok, map()} | {:error, String.t()}
  def get_object(file_system, key) do
    case request(file_system, "/" <> encode_key(key), long: true, decode: false) do
      {:ok, %{status: 200, body: body}} -> {:ok, body}
      {:ok, %{status: 404}} -> FileSystem.Utils.posix_error(:enoent)
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
    case request(file_system, "/" <> encode_key(key), method: :put, body: content, long: true) do
      {:ok, %{status: 200}} -> :ok
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to get the object's ETAG from given key.
  """
  @spec head_object(S3.t(), String.t()) :: {:ok, map()} | {:error, String.t()}
  def head_object(file_system, key) do
    with {:ok, %{status: 200, headers: headers}} <-
           request(file_system, "/" <> encode_key(key), method: :head),
         {:ok, [etag]} <- Map.fetch(headers, "etag") do
      {:ok, %{etag: etag}}
    else
      {:ok, %{status: 404}} -> FileSystem.Utils.posix_error(:enoent)
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

    case request(file_system, "/" <> encode_key(destination_key), method: :put, headers: headers) do
      {:ok, %{status: 200}} -> :ok
      {:ok, %{status: 404}} -> FileSystem.Utils.posix_error(:enoent)
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to delete an object.
  """
  @spec delete_object(S3.t(), String.t()) :: :ok | {:error, String.t()}
  def delete_object(file_system, key) do
    case request(file_system, "/" <> encode_key(key), method: :delete) do
      {:ok, %{status: 204}} -> :ok
      {:ok, %{status: 404}} -> :ok
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
    params = %{"delete" => ""}

    case request(file_system, "/", params: params, method: :post, headers: headers, body: body) do
      {:ok, %{status: 200, body: %{"Error" => _}}} = result -> request_response_to_error(result)
      {:ok, %{status: 200}} -> :ok
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
    params = %{"uploads" => ""}

    case request(file_system, "/" <> encode_key(key), method: :post, params: params, body: "") do
      {:ok, %{status: 200, body: %{"InitiateMultipartUploadResult" => result}}} ->
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
    params = %{"uploadId" => upload_id, "partNumber" => part_number}
    opts = [method: :put, params: params, body: content, long: true]

    with {:ok, %{status: 200, headers: headers}} <-
           request(file_system, "/" <> encode_key(key), opts),
         {:ok, [etag]} <- Map.fetch(headers, "etag") do
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
    params = %{"uploadId" => upload_id}
    parts = for {etag, n} <- Enum.with_index(etags, 1), do: %{"PartNumber" => n, "ETag" => etag}

    body =
      %{"CompleteMultipartUpload" => %{"Part" => parts}}
      |> S3.XML.encode_to_iodata!()
      |> IO.iodata_to_binary()

    case request(file_system, "/" <> encode_key(key), method: :post, params: params, body: body) do
      {:ok, %{status: 200}} -> :ok
      other -> request_response_to_error(other)
    end
  end

  @doc """
  Sends a request to the bucket to abort the multipart object upload.
  """
  @spec abort_multipart_upload(S3.t(), String.t(), String.t()) :: :ok | {:error, String.t()}
  def abort_multipart_upload(file_system, key, upload_id) do
    params = %{"uploadId" => upload_id}

    case request(file_system, "/" <> encode_key(key), method: :delete, params: params) do
      {:ok, %{status: 204}} -> :ok
      other -> request_response_to_error(other)
    end
  end

  defp download(file_system, path, collectable) do
    req =
      Req.new(base_url: file_system.bucket_url)
      |> Livebook.Utils.req_attach_defaults()
      |> with_aws_sigv4(file_system)

    case Req.get(req, url: path, into: collectable) do
      {:ok, %{status: 200, body: collected}} ->
        {:ok, collected}

      {:ok, %{status: status}} ->
        {:error, "download failed, HTTP status #{status}", status}

      {:error, exception} ->
        {:error, "download failed, reason: #{Exception.message(exception)}}", nil}
    end
  end

  defp with_aws_sigv4(req, file_system) do
    credentials = S3.credentials(file_system)

    Req.merge(req,
      aws_sigv4: [
        access_key_id: credentials.access_key_id,
        secret_access_key: credentials.secret_access_key,
        token: credentials.token,
        region: file_system.region,
        service: :s3
      ]
    )
  end

  defp encode_key(key) do
    key
    |> String.split("/")
    |> Enum.map_join("/", fn segment -> URI.encode(segment, &URI.char_unreserved?/1) end)
  end

  defp request(file_system, path, opts) do
    {long, opts} = Keyword.pop(opts, :long, false)
    {decode?, opts} = Keyword.pop(opts, :decode, true)

    timeout = if long, do: 60_000, else: 30_000

    req =
      Req.new(base_url: file_system.bucket_url)
      |> Req.merge(opts)
      |> Livebook.Utils.req_attach_defaults()
      |> with_aws_sigv4(file_system)

    result = Req.request(req, url: path, receive_timeout: timeout)

    if decode?, do: decode(result), else: result
  end

  defp decode({:ok, response}) do
    if xml?(response) do
      {:ok, update_in(response.body, &S3.XML.decode!/1)}
    else
      {:ok, response}
    end
  end

  defp decode({:error, _} = error), do: error

  defp xml?(response) do
    guess_xml? = String.starts_with?(response.body, "<?xml")

    case Livebook.Utils.fetch_content_type(response) do
      {:ok, content_type} when content_type in ["text/xml", "application/xml"] -> true
      # Apparently some requests return XML without content-type
      :error when guess_xml? -> true
      _otherwise -> false
    end
  end

  defp request_response_to_error(
         {:ok, %{status: 403, body: %{"Error" => %{"Message" => message}}}}
       ) do
    {:error, "access denied, " <> Livebook.Utils.downcase_first(message)}
  end

  defp request_response_to_error({:ok, %{status: 403}}) do
    {:error, "access denied"}
  end

  defp request_response_to_error({:ok, %{body: %{"Error" => %{"Message" => message}}}}) do
    {:error, Livebook.Utils.downcase_first(message)}
  end

  defp request_response_to_error({:ok, %{body: %{"Error" => [_ | _] = errors}}}) do
    [%{"Message" => message} | errors] = errors
    {:error, Livebook.Utils.downcase_first(message) <> ", and #{length(errors)} more errors"}
  end

  defp request_response_to_error({:ok, _response}) do
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
