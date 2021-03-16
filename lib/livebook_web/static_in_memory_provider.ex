defmodule LivebookWeb.StaticInMemoryProvider do
  @moduledoc false

  @gzippable_exts ~w(.js .css .txt .text .html .json .svg .eot .ttf)

  # Configurable implementation of `LivebookWeb.StaticProvidedPlug.Provider` behaviour.
  #
  # ## `use` options
  #
  # * `:from` (**required**) - where to read the static files from. See `Plug.Static` for more details.
  #
  # * `:gzip` - whether to bundle gzipped version of the files,
  #   in which case the uncompressed files are not included. Defaults to `false`.

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @behaviour LivebookWeb.StaticProvidedPlug.Provider

      static_path = LivebookWeb.StaticInMemoryProvider.__static_path_from_opts__!(opts)
      paths = LivebookWeb.StaticInMemoryProvider.__paths__(static_path)
      files = LivebookWeb.StaticInMemoryProvider.__preload_files__!(static_path, paths, opts)

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
        current_paths = LivebookWeb.StaticInMemoryProvider.__paths__(unquote(static_path))
        :erlang.md5(current_paths) != unquote(:erlang.md5(paths))
      end
    end
  end

  def __static_path_from_opts__!(opts) do
    from =
      case Keyword.fetch!(opts, :from) do
        {_, _} = from -> from
        from when is_atom(from) -> {from, "priv/static"}
        from when is_binary(from) -> from
        _ -> raise ArgumentError, ":from must be an atom, a binary or a tuple"
      end

    case from do
      {app, from} when is_atom(app) and is_binary(from) ->
        Path.join(Application.app_dir(app), from)

      from ->
        from
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
         %LivebookWeb.StaticProvidedPlug.File{content: gzipped_content, digest: digest}}
      else
        {segments, nil, %LivebookWeb.StaticProvidedPlug.File{content: content, digest: digest}}
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
