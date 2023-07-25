defmodule LivebookWeb.FileSystemWriter do
  @moduledoc false

  # Custom writer for live uploads, uploading directly using the
  # `Livebook.FileSystem` abstraction.
  #
  # ## Options
  #
  #   * `:file` (required) - `%Livebook.FileSystem.File{}` to upload
  #     the contents to
  #

  @behaviour Phoenix.LiveView.UploadWriter

  @impl true
  def init(opts) do
    file = Keyword.fetch!(opts, :file)

    %{file_system: file_system, path: path} = file

    with {:ok, write_state} <- Livebook.FileSystem.write_stream_init(file_system, path, []) do
      {:ok, %{file: file, write_state: write_state}}
    end
  end

  @impl true
  def meta(state) do
    %{file: state.file}
  end

  @impl true
  def write_chunk(chunk, state) do
    case Livebook.FileSystem.write_stream_chunk(state.file.file_system, state.write_state, chunk) do
      {:ok, write_state} -> {:ok, %{state | write_state: write_state}}
      {:error, message} -> {:error, message, state}
    end
  end

  @impl true
  def close(state, :done) do
    case Livebook.FileSystem.write_stream_finish(state.file.file_system, state.write_state) do
      :ok -> {:ok, state}
      {:error, message} -> {:error, message}
    end
  end

  def close(state, _reason) do
    case Livebook.FileSystem.write_stream_halt(state.file.file_system, state.write_state) do
      :ok -> {:ok, state}
      {:error, message} -> {:error, message}
    end
  end
end
