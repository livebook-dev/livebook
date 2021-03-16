defmodule LivebookWeb.FileSystemProvider do
  @moduledoc false

  # Configurable implementation of `LivebookWeb.StaticProvidedPlug.Provider` behaviour,
  # that loads files directly from the file system.
  #
  # ## `use` options
  #
  # * `:from` (**required**) - where to read the static files from. See `Plug.Static` for more details.

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      @behaviour LivebookWeb.StaticProvidedPlug.Provider

      static_path = LivebookWeb.FileSystemProvider.__static_path_from_opts__!(opts)

      @impl true
      def get_file(segments, nil) do
        abs_path = Path.join([unquote(static_path) | segments])
        if File.regular?(abs_path) do
          content = File.read!(abs_path)
          digest = content |> :erlang.md5() |> Base.encode16(case: :lower)
          %LivebookWeb.StaticProvidedPlug.File{content: content, digest: digest}
        else
          nil
        end
      end

      def get_file(_, _), do: nil
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
end
