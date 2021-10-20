defmodule Livebook.Notebook.Explore do
  @moduledoc false

  defmodule NotFoundError do
    @moduledoc false

    defexception [:slug, plug_status: 404]

    def message(%{slug: slug}) do
      "could not find an example notebook matching #{inspect(slug)}"
    end
  end

  @type notebook_info :: %{
          slug: String.t(),
          livemd: String.t(),
          title: String.t(),
          description: String.t(),
          cover_url: String.t(),
          images: images()
        }

  @type images :: %{String.t() => binary()}

  infos = [
    %{
      path: Path.join(__DIR__, "explore/intro_to_livebook.livemd"),
      description: "Get to know Livebook, see how it works and explore its features.",
      cover_url: "/images/logo.png"
    },
    %{
      path: Path.join(__DIR__, "explore/distributed_portals_with_elixir.livemd"),
      description:
        "A fast-paced introduction to the Elixir language by building distributed data-transfer portals.",
      cover_url: "/images/elixir-portal.jpeg",
      image_names: ["portal-drop.jpeg", "portal-list.jpeg"]
    },
    %{
      path: Path.join(__DIR__, "explore/elixir_and_livebook.livemd"),
      description: "Learn how to use some of Elixir and Livebook's unique features together.",
      cover_url: "/images/elixir.png"
    },
    %{
      path: Path.join(__DIR__, "explore/intro_to_vega_lite.livemd"),
      description: "Learn how to quickly create numerous plots for your data.",
      cover_url: "/images/vega_lite.png"
    },
    %{
      path: Path.join(__DIR__, "explore/intro_to_kino.livemd"),
      description: "Display and control rich and interactive widgets in Livebook.",
      cover_url: "/images/kino.png"
    },
    %{
      path: Path.join(__DIR__, "explore/intro_to_nx.livemd"),
      description:
        "Enter Numerical Elixir, experience the power of multi-dimensional arrays of numbers.",
      cover_url: "/images/nx.png"
    },
    # %{
    #   path: Path.join(__DIR__, "explore/intro_to_axon.livemd"),
    #   description: "Build Neural Networks in Elixir using a high-level, composable API.",
    #   cover_url: "/images/axon.png"
    # },
    %{
      path: Path.join(__DIR__, "explore/vm_introspection.livemd"),
      description: "Extract and visualize information about a remote running node.",
      cover_url: "/images/vm_introspection.png"
    }
  ]

  notebook_infos =
    for info <- infos do
      path = Map.fetch!(info, :path)
      @external_resource path

      markdown = File.read!(path)
      # Parse the file to ensure no warnings and read the title.
      # However, in the info we keep just the file contents to save on memory.
      {notebook, []} = Livebook.LiveMarkdown.Import.notebook_from_markdown(markdown)

      images =
        info
        |> Map.get(:image_names, [])
        |> Map.new(fn image_name ->
          path = Path.join([Path.dirname(path), "images", image_name])
          content = File.read!(path)
          {image_name, content}
        end)

      slug = info[:slug] || path |> Path.basename() |> Path.rootname() |> String.replace("_", "-")

      %{
        slug: slug,
        livemd: markdown,
        title: notebook.name,
        description: Map.fetch!(info, :description),
        cover_url: Map.fetch!(info, :cover_url),
        images: images
      }
    end

  @doc """
  Returns a list of example notebooks with metadata.
  """
  @spec notebook_infos() :: list(notebook_info())
  def notebook_infos(), do: unquote(Macro.escape(notebook_infos))

  @doc """
  Finds explore notebook by slug and returns the parsed data structure.

  Returns the notebook along with the images it uses as preloaded binaries.
  """
  @spec notebook_by_slug!(String.t()) :: {Livebook.Notebook.t(), images()}
  def notebook_by_slug!(slug) do
    notebook_infos()
    |> Enum.find(&(&1.slug == slug))
    |> case do
      nil ->
        raise NotFoundError, slug: slug

      notebook_info ->
        {notebook, []} = Livebook.LiveMarkdown.Import.notebook_from_markdown(notebook_info.livemd)
        {notebook, notebook_info.images}
    end
  end
end
