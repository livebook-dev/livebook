defmodule LivebookWeb.StaticInMemoryProvider do
  @moduledoc false

  # Configurable implementation of `LivebookWeb.StaticProvidedPlug.Provider` behaviour.
  #
  # ## `use` options
  #
  # * `:from` (**required**) - where to read the static files from. See `Plug.Static` for more details.
  #
  # * `:only` - filters which files to load. For every static file path
  #   we check if its first part matches one of the filters.

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @behaviour LivebookWeb.StaticProvidedPlug.Provider

      static_path = LivebookWeb.StaticInMemoryProvider.__static_path_from_opts__!(opts)
      paths = LivebookWeb.StaticInMemoryProvider.__paths__(static_path, opts)
      files = LivebookWeb.StaticInMemoryProvider.__preload_files__!(static_path, paths)

      @files files

      @impl true
      def valid_path?(path) do
        Map.has_key?(@files, path)
      end

      @impl true
      def get_file(path) do
        Map.get(@files, path)
      end

      # Force recompilation if the static files change.
      def __mix_recompile__? do
        current_paths =
          LivebookWeb.StaticInMemoryProvider.__paths__(unquote(static_path), unquote(opts))

        current_paths |> Enum.sort() |> :erlang.md5() !=
          unquote(paths |> Enum.sort() |> :erlang.md5())
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

  def __preload_files__!(static_path, paths) do
    Map.new(paths, fn path ->
      abs_path = Path.join(static_path, path)
      content = File.read!(abs_path)
      digest = content |> :erlang.md5() |> Base.encode16(case: :lower)
      {path, %LivebookWeb.StaticProvidedPlug.File{content: content, digest: digest}}
    end)
  end

  def __paths__(static_path, opts) do
    only = Keyword.get(opts, :only, nil)

    paths = static_file_paths!(static_path)
    paths = Enum.filter(paths, &allowed?(only, &1))

    Enum.map(paths, fn file ->
      abs_path = Path.join(static_path, file)

      # If there's a gzipped version, don't include the uncompressed one,
      # as we have to load all the contents into the memory.
      if File.exists?(abs_path <> ".gz") do
        file <> ".gz"
      else
        file
      end
    end)
  end

  defp static_file_paths!(static_path) do
    cache_manifest_path = Path.join(static_path, "cache_manifest.json")
    %{"latest" => latest} = cache_manifest_path |> File.read!() |> Jason.decode!()
    Map.keys(latest)
  end

  defp allowed?(nil, _path), do: true

  defp allowed?(only, path) do
    [part | _] = Path.split(path)
    part in only
  end
end
