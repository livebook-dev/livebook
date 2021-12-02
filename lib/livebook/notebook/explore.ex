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
          images: images(),
          details: details() | nil
        }

  @type images :: %{String.t() => binary()}

  @type details :: %{
          description: String.t(),
          cover_url: String.t()
        }

  images_dir = Path.expand("explore/images", __DIR__)

  welcome_config = %{
    path: Path.join(__DIR__, "explore/intro_to_livebook.livemd"),
    details: %{
      description: "Get to know Livebook, see how it works and explore its features.",
      cover_url: "/images/logo.png"
    }
  }

  other_configs = [
    %{
      path: Path.join(__DIR__, "explore/distributed_portals_with_elixir.livemd"),
      image_paths: [
        Path.join(images_dir, "portal-drop.jpeg"),
        Path.join(images_dir, "portal-list.jpeg")
      ],
      details: %{
        description:
          "A fast-paced introduction to the Elixir language by building distributed data-transfer portals.",
        cover_url: "/images/elixir-portal.jpeg"
      }
    },
    %{
      path: Path.join(__DIR__, "explore/elixir_and_livebook.livemd"),
      details: %{
        description: "Learn how to use some of Elixir and Livebook's unique features together.",
        cover_url: "/images/elixir.png"
      }
    },
    %{
      path: Path.join(__DIR__, "explore/intro_to_vega_lite.livemd"),
      details: %{
        description: "Learn how to quickly create numerous plots for your data.",
        cover_url: "/images/vega_lite.png"
      }
    },
    %{
      path: Path.join(__DIR__, "explore/intro_to_kino.livemd"),
      details: %{
        description: "Display and control rich and interactive widgets in Livebook.",
        cover_url: "/images/kino.png"
      }
    },
    %{
      path: Path.join(__DIR__, "explore/intro_to_nx.livemd"),
      details: %{
        description:
          "Enter Numerical Elixir, experience the power of multi-dimensional arrays of numbers.",
        cover_url: "/images/nx.png"
      }
    },
    # %{
    #   path: Path.join(__DIR__, "explore/intro_to_axon.livemd"),
    #   details: %{
    #     description: "Build Neural Networks in Elixir using a high-level, composable API.",
    #     cover_url: "/images/axon.png"
    #   }
    # },
    %{
      path: Path.join(__DIR__, "explore/vm_introspection.livemd"),
      details: %{
        description: "Extract and visualize information about a remote running node.",
        cover_url: "/images/vm_introspection.png"
      }
    },
    %{
      path: Path.join(__DIR__, "explore/pong.livemd"),
      details: %{
        description: "Implement and play multiplayer Pong directly in Livebook.",
        cover_url: "/images/pong.png"
      }
    }
  ]

  user_configs = Application.fetch_env!(:livebook, :explore_notebooks)

  notebook_configs = [welcome_config] ++ user_configs ++ other_configs

  notebook_infos =
    for config <- notebook_configs do
      path =
        config[:path] ||
          raise "missing required :path attribute in notebook configuration: #{inspect(config)}"

      @external_resource path

      markdown = File.read!(path)
      # Parse the file to ensure no warnings and read the title.
      # However, in the info we keep just the file contents to save on memory.
      {notebook, warnings} = Livebook.LiveMarkdown.Import.notebook_from_markdown(markdown)

      if warnings != [] do
        items = Enum.map(warnings, &("- " <> &1))
        raise "found warnings while importing #{path}:\n\n" <> Enum.join(items, "\n")
      end

      images =
        config
        |> Map.get(:image_paths, [])
        |> Map.new(fn image_path ->
          image_name = Path.basename(image_path)
          content = File.read!(image_path)
          {image_name, content}
        end)

      slug =
        config[:slug] || path |> Path.basename() |> Path.rootname() |> String.replace("_", "-")

      %{
        slug: slug,
        livemd: markdown,
        title: notebook.name,
        images: images,
        details:
          if config_details = config[:details] do
            description =
              config_details[:description] ||
                raise "missing required :description attribute in notebook details: #{inspect(config_details)}"

            cover_url =
              config_details[:cover_url] ||
                (config_details[:cover_path] &&
                   Livebook.Utils.read_as_data_url!(config_details.cover_path)) ||
                raise "expected either :cover_path or :cover_url in notebooks details: #{inspect(config_details)}"

            %{description: description, cover_url: cover_url}
          end
      }
    end

  @doc """
  Returns a list of example notebooks with metadata.
  """
  @spec notebook_infos() :: list(notebook_info())
  def notebook_infos(), do: unquote(Macro.escape(notebook_infos))

  @doc """
  Same as `notebook_infos/0`, but returns only notebooks that have
  additional details.
  """
  @spec visible_notebook_infos() :: list(notebook_info())
  def visible_notebook_infos() do
    notebook_infos() |> Enum.filter(& &1.details)
  end

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
