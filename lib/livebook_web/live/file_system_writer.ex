defmodule LivebookWeb.FileSystemWriter do
  # Custom writer for live uploads, uploading directly using the
  # `Livebook.FileSystem` abstraction.
  #
  # ## Options
  #
  #   * `:file` (required) - `%Livebook.FileSystem.File{}` to upload
  #     the contents to
  #

  @behaviour Phoenix.LiveView.UploadWriter

  alias Livebook.FileSystem

  @impl true
  def init(opts) do
    file = Keyword.fetch!(opts, :file)

    with {:ok, file_system} <- FileSystem.File.fetch_file_system(file),
         {:ok, write_state} <- FileSystem.write_stream_init(file_system, file.path, []) do
      {:ok, %{file: file, file_system: file_system, write_state: write_state}}
    end
  end

  @impl true
  def meta(state) do
    %{file: state.file}
  end

  @impl true
  def write_chunk(chunk, state) do
    case FileSystem.write_stream_chunk(state.file_system, state.write_state, chunk) do
      {:ok, write_state} -> {:ok, %{state | write_state: write_state}}
      {:error, message} -> {:error, message, state}
    end
  end

  @impl true
  def close(state, :done) do
    case FileSystem.write_stream_finish(state.file_system, state.write_state) do
      :ok -> {:ok, state}
      {:error, message} -> {:error, message}
    end
  end

  def close(state, _reason) do
    case FileSystem.write_stream_halt(state.file_system, state.write_state) do
      :ok -> {:ok, state}
      {:error, message} -> {:error, message}
    end
  end
end
