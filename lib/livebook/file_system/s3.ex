defmodule Livebook.FileSystem.S3 do
  @moduledoc false

  # File system backed by an S3 bucket.

  defstruct [:id, :bucket_url, :region, :access_key_id, :secret_access_key]

  @type t :: %__MODULE__{
          id: String.t(),
          bucket_url: String.t(),
          region: String.t(),
          access_key_id: String.t(),
          secret_access_key: String.t()
        }

  @doc """
  Returns a new file system struct.

  ## Options

    * `:region` - the bucket region. By default the URL is assumed
      to have the format `*.[region].[rootdomain].com` and the region
      is inferred from that URL

  """
  @spec new(String.t(), String.t(), String.t(), keyword()) :: t()
  def new(bucket_url, access_key_id, secret_access_key, opts \\ []) do
    opts = Keyword.validate!(opts, [:region])

    bucket_url = String.trim_trailing(bucket_url, "/")
    region = opts[:region] || region_from_uri(bucket_url)

    hash = :crypto.hash(:sha256, bucket_url) |> Base.url_encode64(padding: false)
    id = "s3-#{hash}"

    %__MODULE__{
      id: id,
      bucket_url: bucket_url,
      region: region,
      access_key_id: access_key_id,
      secret_access_key: secret_access_key
    }
  end

  defp region_from_uri(uri) do
    # For many services the API host is of the form *.[region].[rootdomain].com
    %{host: host} = URI.parse(uri)
    host |> String.split(".") |> Enum.reverse() |> Enum.at(2, "auto")
  end

  @doc """
  Parses file system from a configuration map.
  """
  @spec from_config(map()) :: {:ok, t()} | {:error, String.t()}
  def from_config(config) do
    case config do
      %{
        bucket_url: bucket_url,
        access_key_id: access_key_id,
        secret_access_key: secret_access_key
      } ->
        file_system = new(bucket_url, access_key_id, secret_access_key, region: config[:region])
        {:ok, file_system}

      _config ->
        {:error,
         "S3 file system config is expected to have keys: :bucket_url, :access_key_id and :secret_access_key, but got #{inspect(config)}"}
    end
  end

  @spec to_config(t()) :: map()
  def to_config(%__MODULE__{} = s3) do
    Map.take(s3, [:bucket_url, :region, :access_key_id, :secret_access_key])
  end
end

defimpl Livebook.FileSystem, for: Livebook.FileSystem.S3 do
  alias Livebook.FileSystem
  alias Livebook.Utils.HTTP
  alias Livebook.FileSystem.S3.XML

  def resource_identifier(file_system) do
    {:s3, file_system.bucket_url}
  end

  def type(_file_system) do
    :global
  end

  def default_path(_file_system) do
    "/"
  end

  def list(file_system, path, recursive) do
    FileSystem.Utils.assert_dir_path!(path)
    "/" <> dir_key = path

    delimiter = if recursive, do: nil, else: "/"

    with {:ok, %{keys: keys}} <- list_objects(file_system, prefix: dir_key, delimiter: delimiter) do
      if keys == [] and dir_key != "" do
        FileSystem.Utils.posix_error(:enoent)
      else
        paths = keys |> List.delete(dir_key) |> Enum.map(&("/" <> &1))
        {:ok, paths}
      end
    end
  end

  def read(file_system, path) do
    FileSystem.Utils.assert_regular_path!(path)
    "/" <> key = path
    get_object(file_system, key)
  end

  def write(file_system, path, content) do
    FileSystem.Utils.assert_regular_path!(path)
    "/" <> key = path
    put_object(file_system, key, content)
  end

  def access(_file_system, _path) do
    {:ok, :read_write}
  end

  def create_dir(file_system, path) do
    FileSystem.Utils.assert_dir_path!(path)
    "/" <> key = path
    # S3 has no concept of directories, but keys with trailing
    # slash are interpreted as such, so we create an empty
    # object for the given key
    put_object(file_system, key, nil)
  end

  def remove(file_system, path) do
    "/" <> key = path

    if FileSystem.Utils.dir_path?(path) do
      with {:ok, %{keys: keys}} <- list_objects(file_system, prefix: key) do
        if keys == [] do
          FileSystem.Utils.posix_error(:enoent)
        else
          delete_objects(file_system, keys)
        end
      end
    else
      delete_object(file_system, key)
    end
  end

  def copy(file_system, source_path, destination_path) do
    FileSystem.Utils.assert_same_type!(source_path, destination_path)
    "/" <> source_key = source_path
    "/" <> destination_key = destination_path

    if FileSystem.Utils.dir_path?(source_path) do
      with {:ok, %{bucket: bucket, keys: keys}} <- list_objects(file_system, prefix: source_key) do
        if keys == [] do
          FileSystem.Utils.posix_error(:enoent)
        else
          keys
          |> Enum.map(fn key ->
            renamed_key = String.replace_prefix(key, source_key, destination_key)

            Task.async(fn ->
              copy_object(file_system, bucket, key, renamed_key)
            end)
          end)
          |> Task.await_many(:infinity)
          |> Enum.filter(&match?({:error, _}, &1))
          |> case do
            [] -> :ok
            [error | _] -> error
          end
        end
      end
    else
      with {:ok, bucket} <- get_bucket_name(file_system) do
        copy_object(file_system, bucket, source_key, destination_key)
      end
    end
  end

  def rename(file_system, source_path, destination_path) do
    FileSystem.Utils.assert_same_type!(source_path, destination_path)
    "/" <> destination_key = destination_path

    with {:ok, destination_exists?} <- object_exists(file_system, destination_key) do
      if destination_exists? do
        FileSystem.Utils.posix_error(:eexist)
      else
        # S3 doesn't support an atomic rename/move operation,
        # so we implement it as copy followed by remove
        with :ok <- copy(file_system, source_path, destination_path) do
          remove(file_system, source_path)
        end
      end
    end
  end

  def etag_for(file_system, path) do
    FileSystem.Utils.assert_regular_path!(path)
    "/" <> key = path

    with {:ok, %{etag: etag}} <- head_object(file_system, key) do
      {:ok, etag}
    end
  end

  def exists?(file_system, path) do
    "/" <> key = path
    object_exists(file_system, key)
  end

  def resolve_path(_file_system, dir_path, subject) do
    FileSystem.Utils.resolve_unix_like_path(dir_path, subject)
  end

  def write_stream_init(_file_system, path, opts) do
    opts = Keyword.validate!(opts, part_size: 50_000_000)

    FileSystem.Utils.assert_regular_path!(path)
    "/" <> key = path

    {:ok,
     %{
       key: key,
       parts: 0,
       etags: [],
       current_chunks: [],
       current_size: 0,
       part_size: opts[:part_size],
       upload_id: nil
     }}
  end

  def write_stream_chunk(file_system, state, chunk) when is_binary(chunk) do
    chunk_size = byte_size(chunk)

    state = update_in(state.current_size, &(&1 + chunk_size))
    state = update_in(state.current_chunks, &[chunk | &1])

    if state.current_size >= state.part_size do
      maybe_state =
        if state.upload_id do
          {:ok, state}
        else
          with {:ok, upload_id} <- create_multipart_upload(file_system, state.key) do
            {:ok, %{state | upload_id: upload_id}}
          end
        end

      with {:ok, state} <- maybe_state do
        upload_part_from_state(file_system, state, state.part_size)
      end
    else
      {:ok, state}
    end
  end

  defp upload_part_from_state(file_system, state, part_size) do
    <<part::binary-size(part_size), rest::binary>> =
      state.current_chunks
      |> Enum.reverse()
      |> IO.iodata_to_binary()

    parts = state.parts + 1

    with {:ok, %{etag: etag}} <- upload_part(file_system, state.key, state.upload_id, parts, part) do
      {:ok,
       %{
         state
         | current_chunks: [rest],
           current_size: byte_size(rest),
           etags: [etag | state.etags],
           parts: parts
       }}
    end
  end

  def write_stream_finish(file_system, state) do
    if state.upload_id do
      maybe_state =
        if state.current_size > 0 do
          upload_part_from_state(file_system, state, state.current_size)
        else
          {:ok, state}
        end

      with {:ok, state} <- maybe_state,
           :ok <-
             complete_multipart_upload(
               file_system,
               state.key,
               state.upload_id,
               Enum.reverse(state.etags)
             ) do
        :ok
      else
        {:error, error} ->
          abort_multipart_upload(file_system, state.key, state.upload_id)
          {:error, error}
      end
    else
      content = state.current_chunks |> Enum.reverse() |> IO.iodata_to_binary()
      put_object(file_system, state.key, content)
    end
  end

  def write_stream_halt(file_system, state) do
    if state.upload_id do
      abort_multipart_upload(file_system, state.key, state.upload_id)
    else
      :ok
    end
  end

  def read_stream_into(file_system, path, collectable) do
    FileSystem.Utils.assert_regular_path!(path)
    "/" <> key = path
    multipart_get_object(file_system, key, collectable)
  end

  # Requests

  defp list_objects(file_system, opts) do
    prefix = opts[:prefix]
    delimiter = opts[:delimiter]

    query = %{"list-type" => "2", "prefix" => prefix, "delimiter" => delimiter}

    case request(file_system, :get, "/", query: query) |> decode() do
      {:ok, 200, _headers, %{"ListBucketResult" => result}} ->
        bucket = result["Name"]
        file_keys = result |> xml_get_list("Contents") |> Enum.map(& &1["Key"])
        prefix_keys = result |> xml_get_list("CommonPrefixes") |> Enum.map(& &1["Prefix"])
        keys = file_keys ++ prefix_keys
        {:ok, %{bucket: bucket, keys: keys}}

      other ->
        request_response_to_error(other)
    end
  end

  defp get_bucket_name(file_system) do
    # We have bucket URL, but it's not straightforward to extract
    # bucket name from the URL, because it may be either the path
    # or a part of the host.
    #
    # Endpoints that return bucket information doesn't include the
    # name, but the listing endpoint does, so we just list keys
    # with an upper limit of 0 and retrieve the bucket name.

    query = %{"list-type" => "2", "max-keys" => "0"}

    case request(file_system, :get, "/", query: query) |> decode() do
      {:ok, 200, _headers, %{"ListBucketResult" => %{"Name" => bucket}}} ->
        {:ok, bucket}

      other ->
        request_response_to_error(other)
    end
  end

  defp get_object(file_system, key) do
    case request(file_system, :get, "/" <> encode_key(key), long: true) do
      {:ok, 200, _headers, body} -> {:ok, body}
      {:ok, 404, _headers, _body} -> FileSystem.Utils.posix_error(:enoent)
      other -> request_response_to_error(other)
    end
  end

  defp multipart_get_object(file_system, key, collectable) do
    case download(file_system, "/" <> encode_key(key), collectable) do
      {:ok, collectable} -> {:ok, collectable}
      {:error, _message, 404} -> FileSystem.Utils.posix_error(:enoent)
      {:error, message, _status} -> {:error, message}
    end
  end

  defp put_object(file_system, key, content) do
    case request(file_system, :put, "/" <> encode_key(key), body: content, long: true)
         |> decode() do
      {:ok, 200, _headers, _body} -> :ok
      other -> request_response_to_error(other)
    end
  end

  defp head_object(file_system, key) do
    case request(file_system, :head, "/" <> encode_key(key)) do
      {:ok, 200, headers, _body} ->
        {:ok, etag} = HTTP.fetch_header(headers, "etag")
        {:ok, %{etag: etag}}

      {:ok, 404, _headers, _body} ->
        FileSystem.Utils.posix_error(:enoent)

      other ->
        request_response_to_error(other)
    end
  end

  defp copy_object(file_system, bucket, source_key, destination_key) do
    copy_source = bucket <> "/" <> encode_key(source_key)

    headers = [{"x-amz-copy-source", copy_source}]

    case request(file_system, :put, "/" <> encode_key(destination_key), headers: headers)
         |> decode() do
      {:ok, 200, _headers, _body} -> :ok
      {:ok, 404, _headers, _body} -> FileSystem.Utils.posix_error(:enoent)
      other -> request_response_to_error(other)
    end
  end

  defp delete_object(file_system, key) do
    case request(file_system, :delete, "/" <> encode_key(key)) |> decode() do
      {:ok, 204, _headers, _body} -> :ok
      {:ok, 404, _headers, _body} -> :ok
      other -> request_response_to_error(other)
    end
  end

  defp delete_objects(file_system, keys) do
    objects = Enum.map(keys, fn key -> %{"Key" => key} end)

    body =
      %{"Delete" => %{"Object" => objects, "Quiet" => "true"}}
      |> XML.encode_to_iodata!()
      |> IO.iodata_to_binary()

    body_md5 = :crypto.hash(:md5, body) |> Base.encode64()

    headers = [{"Content-MD5", body_md5}]

    case request(file_system, :post, "/", query: %{"delete" => ""}, headers: headers, body: body)
         |> decode() do
      {:ok, 200, _headers, %{"Error" => errors}} -> {:error, format_errors(errors)}
      {:ok, 200, _headers, _body} -> :ok
      other -> request_response_to_error(other)
    end
  end

  defp object_exists(file_system, key) do
    # It is possible for /dir/obj to exist without the /dir/ object,
    # but we still consider it as existing. That's why we list
    # objects instead of checking the key directly.

    with {:ok, %{keys: keys}} <- list_objects(file_system, prefix: key, delimiter: "/") do
      exists? =
        if String.ends_with?(key, "/") do
          keys != []
        else
          key in keys
        end

      {:ok, exists?}
    end
  end

  defp create_multipart_upload(file_system, key) do
    query = %{"uploads" => ""}

    case request(file_system, :post, "/" <> encode_key(key), query: query, body: "")
         |> decode() do
      {:ok, 200, _headers, %{"InitiateMultipartUploadResult" => %{"UploadId" => upload_id}}} ->
        {:ok, upload_id}

      other ->
        request_response_to_error(other)
    end
  end

  defp upload_part(file_system, key, upload_id, part_number, content) do
    query = %{"uploadId" => upload_id, "partNumber" => part_number}

    case request(file_system, :put, "/" <> encode_key(key),
           query: query,
           body: content,
           long: true
         )
         |> decode() do
      {:ok, 200, headers, _body} ->
        {:ok, etag} = HTTP.fetch_header(headers, "etag")
        {:ok, %{etag: etag}}

      other ->
        request_response_to_error(other)
    end
  end

  defp complete_multipart_upload(file_system, key, upload_id, etags) do
    query = %{"uploadId" => upload_id}

    parts =
      for {etag, n} <- Enum.with_index(etags, 1) do
        %{"PartNumber" => n, "ETag" => etag}
      end

    body =
      %{"CompleteMultipartUpload" => %{"Part" => parts}}
      |> XML.encode_to_iodata!()
      |> IO.iodata_to_binary()

    case request(file_system, :post, "/" <> encode_key(key), query: query, body: body)
         |> decode() do
      {:ok, 200, _headers, _body} -> :ok
      other -> request_response_to_error(other)
    end
  end

  defp abort_multipart_upload(file_system, key, upload_id) do
    query = %{"uploadId" => upload_id}

    case request(file_system, :delete, "/" <> encode_key(key), query: query) |> decode() do
      {:ok, 204, _headers, _body} -> :ok
      other -> request_response_to_error(other)
    end
  end

  defp encode_key(key) do
    key
    |> String.split("/")
    |> Enum.map_join("/", fn segment -> URI.encode(segment, &URI.char_unreserved?/1) end)
  end

  defp request_response_to_error(error)

  defp request_response_to_error({:ok, 403, _headers, %{"Error" => %{"Message" => message}}}) do
    {:error, "access denied, " <> Livebook.Utils.downcase_first(message)}
  end

  defp request_response_to_error({:ok, 403, _headers, _body}) do
    {:error, "access denied"}
  end

  defp request_response_to_error({:ok, _status, _headers, %{"Error" => error}}) do
    {:error, format_errors(error)}
  end

  defp request_response_to_error({:ok, _status, _headers, _body}) do
    {:error, "unexpected response"}
  end

  defp request_response_to_error({:error, _error}) do
    {:error, "failed to make an HTTP request"}
  end

  defp format_errors(%{"Message" => message}) do
    Livebook.Utils.downcase_first(message)
  end

  defp format_errors([%{"Message" => message} | errors]) do
    Livebook.Utils.downcase_first(message) <> ", and #{length(errors)} more errors"
  end

  defp request(file_system, method, path, opts \\ []) do
    query = opts[:query] || %{}
    headers = opts[:headers] || []
    body = opts[:body]
    long = Keyword.get(opts, :long, false)

    timeout_opts = if(long, do: [timeout: 60_000], else: [])

    url = url(file_system, path, query)
    headers = headers(file_system, method, url, headers, body)
    body = body && {"application/octet-stream", body}

    HTTP.request(method, url, [headers: headers, body: body] ++ timeout_opts)
  end

  defp download(file_system, path, collectable, opts \\ []) do
    query = opts[:query] || %{}
    headers = opts[:headers] || []

    url = url(file_system, path, query)
    headers = headers(file_system, :get, url, headers)

    HTTP.download(url, collectable, headers: headers)
  end

  defp url(file_system, path, query) do
    file_system.bucket_url <> path <> "?" <> URI.encode_query(query)
  end

  defp headers(file_system, method, url, headers, body \\ nil) do
    now = NaiveDateTime.utc_now() |> NaiveDateTime.to_erl()
    %{host: host} = URI.parse(file_system.bucket_url)
    headers = [{"Host", host} | headers]

    :aws_signature.sign_v4(
      file_system.access_key_id,
      file_system.secret_access_key,
      file_system.region,
      "s3",
      now,
      Atom.to_string(method),
      url,
      headers,
      body || "",
      uri_encode_path: false
    )
  end

  defp decode({:ok, status, headers, body}) do
    guess_xml? = String.starts_with?(body, "<?xml")

    case HTTP.fetch_content_type(headers) do
      {:ok, content_type} when content_type in ["text/xml", "application/xml"] ->
        {:ok, status, headers, XML.decode!(body)}

      # Apparently some requests return XML without content-type
      :error when guess_xml? ->
        {:ok, status, headers, XML.decode!(body)}

      _ ->
        {:ok, status, headers, body}
    end
  end

  defp decode(other), do: other

  defp xml_get_list(xml_map, key) do
    case xml_map do
      %{^key => item} when is_map(item) -> [item]
      %{^key => items} when is_list(items) -> items
      _ -> []
    end
  end
end
