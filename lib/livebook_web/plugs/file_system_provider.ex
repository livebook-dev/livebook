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

      from = Keyword.fetch!(opts, :from)
      static_path = LivebookWeb.StaticProvidedPlug.Provider.static_path(from)

      @impl true
      def get_file(segments, compression) do
        LivebookWeb.FileSystemProvider.__get_file__(unquote(static_path), segments, compression)
      end
    end
  end

  def __get_file__(static_path, segments, nil) do
    abs_path = Path.join([static_path | segments])

    if File.regular?(abs_path) do
      content = File.read!(abs_path)
      digest = content |> :erlang.md5() |> Base.encode16(case: :lower)
      %LivebookWeb.StaticProvidedPlug.File{content: content, digest: digest}
    else
      nil
    end
  end

  def __get_file__(_static_path, _segments, _compression), do: nil
end
