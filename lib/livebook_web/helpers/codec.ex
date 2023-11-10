defmodule LivebookWeb.Helpers.Codec do
  @wav_header_size 44

  @doc """
  Returns the size of a WAV binary that would wrap PCM data of the
  given size.

  This size matches the result of `encode_pcm_as_wav_stream!/6`.
  """
  @spec pcm_as_wav_size(pos_integer()) :: pos_integer()
  def pcm_as_wav_size(pcm_size) do
    @wav_header_size + pcm_size
  end

  @doc """
  Encodes PCM float-32 in native endianness into a WAV binary.

  Accepts a range of the WAV binary that should be returned. Returns
  a stream, where the PCM binary is streamed from the given file.
  """
  @spec encode_pcm_as_wav_stream!(
          Path.t(),
          non_neg_integer(),
          pos_integer(),
          pos_integer(),
          non_neg_integer(),
          pos_integer()
        ) :: Enumerable.t()
  def encode_pcm_as_wav_stream!(path, file_size, num_channels, sampling_rate, offset, length) do
    {header_enum, file_length} =
      if offset < @wav_header_size do
        header = encode_pcm_as_wav_header(file_size, num_channels, sampling_rate)
        header_length = min(@wav_header_size - offset, length)
        header_slice = binary_slice(header, offset, header_length)
        {[header_slice], length - header_length}
      else
        {[], length}
      end

    file_offset = max(offset - @wav_header_size, 0)

    # TODO: use File.stream!(path, [{:read_offset, file_offset}]) once we require Elixir v1.16+
    file_stream = raw_file_range_stream!(path, file_offset, file_length)

    file_stream =
      case System.endianness() do
        :little ->
          file_stream

        :big ->
          Stream.map(file_stream, fn binary ->
            for <<x::32-float-big <- binary>>, reduce: <<>> do
              acc -> <<acc::binary, x::32-float-little>>
            end
          end)
      end

    Stream.concat(header_enum, file_stream)
  end

  defp encode_pcm_as_wav_header(pcm_size, num_channels, sampling_rate) do
    # See http://soundfile.sapp.org/doc/WaveFormat

    num_frames = div(pcm_size, 4)
    bytes_per_sample = 4

    block_align = num_channels * bytes_per_sample
    byte_rate = sampling_rate * block_align
    data_size = num_frames * block_align

    <<
      "RIFF",
      36 + data_size::32-unsigned-integer-little,
      "WAVE",
      "fmt ",
      16::32-unsigned-integer-little,
      # 3 indicates 32-bit float PCM
      3::16-unsigned-integer-little,
      num_channels::16-unsigned-integer-little,
      sampling_rate::32-unsigned-integer-little,
      byte_rate::32-unsigned-integer-little,
      block_align::16-unsigned-integer-little,
      bytes_per_sample * 8::16-unsigned-integer-little,
      "data",
      data_size::32-unsigned-integer-little
    >>
  end

  # We assume a local path and open a raw file for efficiency
  defp raw_file_range_stream!(path, offset, length) do
    chunk_size = 64_000

    Stream.resource(
      fn ->
        {:ok, fd} = :file.open(path, [:raw, :binary, :read, :read_ahead])
        {:ok, _} = :file.position(fd, offset)
        {fd, length}
      end,
      fn
        {fd, 0} ->
          {:halt, {fd, 0}}

        {fd, length} ->
          size = min(chunk_size, length)
          {:ok, chunk} = :file.read(fd, size)
          {[chunk], {fd, length - size}}
      end,
      fn {fd, _} -> :file.close(fd) end
    )
  end

  @doc """
  Builds a single binary with JSON-serialized `meta` and `binary`.
  """
  @spec encode_annotated_binary!(term(), binary()) :: binary()
  def encode_annotated_binary!(meta, binary) do
    meta = Jason.encode!(meta)
    meta_size = byte_size(meta)
    <<meta_size::size(32), meta::binary, binary::binary>>
  end

  @doc """
  Decodes binary annotated with JSON-serialized metadata.
  """
  @spec decode_annotated_binary!(binary()) :: {term(), binary()}
  def decode_annotated_binary!(raw) do
    <<meta_size::size(32), meta::binary-size(meta_size), binary::binary>> = raw
    meta = Jason.decode!(meta)
    {meta, binary}
  end
end
