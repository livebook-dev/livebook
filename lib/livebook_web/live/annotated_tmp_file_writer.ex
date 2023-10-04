defmodule LivebookWeb.AnnotatedTmpFileWriter do
  # Custom writer for JSON-annotated binary.
  #
  # This corresponds to `LivebookWeb.Helpers.Codec.decode_annotated_binary!/1`.
  # The entry metadata include `:meta` key with the annotation payload,
  # while the actual binary is written to a temporary file, the same
  # way the default `Phoenix.LiveView.UploadTmpFileWriter` would do.

  @behaviour Phoenix.LiveView.UploadWriter

  @impl true
  def init(_opts) do
    with {:ok, path} <- Plug.Upload.random_file("live_view_upload"),
         {:ok, file} <- File.open(path, [:binary, :write]) do
      {:ok, %{meta_size: nil, meta_binary: <<>>, path: path, file: file}}
    end
  end

  @impl true
  def meta(state) do
    meta = Jason.decode!(state.meta_binary)
    %{meta: meta, path: state.path}
  end

  @impl true
  def write_chunk(data, %{meta_size: nil} = state) do
    <<meta_size::size(32), rest::binary>> = data
    write_chunk(rest, %{state | meta_size: meta_size})
  end

  def write_chunk(data, state) when byte_size(state.meta_binary) < state.meta_size do
    data_size = byte_size(data)
    pending_meta = state.meta_size - byte_size(state.meta_binary)

    if data_size > pending_meta do
      left = binary_part(data, 0, pending_meta)
      right = binary_slice(data, pending_meta..-1//1)
      write_chunk(right, %{state | meta_binary: <<state.meta_binary::binary, left::binary>>})
    else
      {:ok, %{state | meta_binary: <<state.meta_binary::binary, data::binary>>}}
    end
  end

  def write_chunk(data, state) do
    case IO.binwrite(state.file, data) do
      :ok -> {:ok, state}
      {:error, reason} -> {:error, reason, state}
    end
  end

  @impl true
  def close(state, _reason) do
    case File.close(state.file) do
      :ok -> {:ok, state}
      {:error, reason} -> {:error, reason}
    end
  end
end
