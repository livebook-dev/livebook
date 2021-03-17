defmodule LivebookWeb.MemoryProvider do
  @moduledoc false

  @gzippable_exts ~w(.js .css .txt .text .html .json .svg .eot .ttf)

  # Configurable implementation of `LivebookWeb.StaticPlug.Provider` behaviour,
  # that bundles the files into the module compiled source.
  #
  # ## `use` options
  #
  # * `:from` (**required**) - where to read the static files from. See `Plug.Static` for more details.
  #
  # * `:gzip` - whether to bundle gzipped version of the files,
  #   in which case the uncompressed files are not included. Defaults to `false`.

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @behaviour LivebookWeb.StaticPlug.Provider

      from = Keyword.fetch!(opts, :from)
      static_path = LivebookWeb.StaticPlug.Provider.static_path(from)
      paths = LivebookWeb.MemoryProvider.__paths__(static_path)
      files = LivebookWeb.MemoryProvider.__preload_files__!(static_path, paths, opts)

      for path <- paths do
        abs_path = Path.join(static_path, path)
        @external_resource Path.relative_to_cwd(abs_path)
      end

      @impl true
      def get_file(segments, compression)

      for {segments, compression, file} <- files do
        def get_file(unquote(segments), unquote(compression)), do: unquote(Macro.escape(file))
      end

      def get_file(_, _), do: nil

      # Force recompilation if the static files change.
      def __mix_recompile__? do
        current_paths = LivebookWeb.MemoryProvider.__paths__(unquote(static_path))
        :erlang.md5(current_paths) != unquote(:erlang.md5(paths))
      end
    end
  end

  def __preload_files__!(static_path, paths, opts) do
    gzip? = Keyword.get(opts, :gzip, false)

    Enum.map(paths, fn path ->
      segments = Path.split(path)
      abs_path = Path.join(static_path, path)
      content = File.read!(abs_path)
      digest = content |> :erlang.md5() |> Base.encode16(case: :lower)

      if gzip? and Path.extname(path) in @gzippable_exts do
        gzipped_content = :zlib.gzip(content)

        {segments, :gzip,
         %LivebookWeb.StaticPlug.File{content: gzipped_content, digest: digest}}
      else
        {segments, nil, %LivebookWeb.StaticPlug.File{content: content, digest: digest}}
      end
    end)
  end

  def __paths__(static_path) do
    Path.join(static_path, "**")
    |> Path.wildcard()
    |> Enum.reject(&File.dir?/1)
    |> Enum.map(&String.replace_leading(&1, static_path <> "/", ""))
    |> Enum.sort()
  end
end
