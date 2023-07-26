defmodule Livebook.Notebook.Learn do
  @moduledoc false

  defmodule NotFoundError do
    @moduledoc false

    defexception [:slug, plug_status: 404]

    def message(%{slug: slug}) do
      "could not find an example notebook matching #{inspect(slug)}"
    end
  end

  @type notebook_info :: %{
          ref: atom() | nil,
          slug: String.t(),
          livemd: String.t(),
          title: String.t(),
          files: files(),
          details: details() | nil
        }

  @type files :: %{String.t() => binary()}

  @type details :: %{
          description: String.t(),
          cover: image_source()
        }

  @type group_info :: %{
          title: String.t(),
          description: String.t(),
          cover: image_source(),
          notebook_infos: list(notebook_info())
        }

  @type image_source :: {:static, filename :: String.t()} | {:url, String.t()}

  files_dir = Path.expand("learn/files", __DIR__)

  welcome_config = %{
    path: Path.join(__DIR__, "learn/intro_to_livebook.livemd"),
    details: %{
      description: "Get to know Livebook, see how it works, and learn its features.",
      cover_filename: "logo.png"
    }
  }

  other_configs = [
    %{
      path: Path.join(__DIR__, "learn/distributed_portals_with_elixir.livemd"),
      file_paths: [
        Path.join(files_dir, "portal-drop.jpeg"),
        Path.join(files_dir, "portal-list.jpeg")
      ],
      details: %{
        description:
          "A fast-paced introduction to Elixir by building distributed data-transfer portals.",
        cover_filename: "elixir.png"
      }
    },
    %{
      path: Path.join(__DIR__, "learn/deploy_apps.livemd"),
      details: %{
        description: "Write and deploy a chat app with Kino control and frames.",
        cover_filename: "learn-deploy.svg"
      }
    },
    %{
      path: Path.join(__DIR__, "learn/intro_to_explorer.livemd"),
      details: %{
        description: "Intuitive data visualizations and data pipelines on the fly.",
        cover_filename: "explorer.png"
      }
    },
    %{
      path: Path.join(__DIR__, "learn/intro_to_vega_lite.livemd"),
      details: %{
        description: "Learn how to quickly create numerous plots for your data.",
        cover_filename: "vega_lite.png"
      }
    },
    %{
      path: Path.join(__DIR__, "learn/intro_to_maplibre.livemd"),
      details: %{
        description: "Seamlessly plot maps using geospatial and tabular data.",
        cover_filename: "maplibre.png"
      }
    },
    %{
      ref: :kino_intro,
      path: Path.join(__DIR__, "learn/kino/intro_to_kino.livemd")
    },
    %{
      ref: :kino_vm_introspection,
      path: Path.join(__DIR__, "learn/kino/vm_introspection.livemd")
    },
    %{
      ref: :kino_custom_kinos,
      path: Path.join(__DIR__, "learn/kino/custom_kinos.livemd")
    },
    %{
      ref: :kino_smart_cells,
      path: Path.join(__DIR__, "learn/kino/smart_cells.livemd")
    }
  ]

  user_configs = Application.compile_env(:livebook, :learn_notebooks, [])

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
      {notebook, warnings} = Livebook.LiveMarkdown.notebook_from_livemd(markdown)

      if warnings != [] do
        items = Enum.map(warnings, &("- " <> &1))
        raise "found warnings while importing #{path}:\n\n" <> Enum.join(items, "\n")
      end

      files =
        config
        |> Map.get(:file_paths, [])
        |> Map.new(fn image_path ->
          image_name = Path.basename(image_path)
          content = File.read!(image_path)
          {image_name, content}
        end)

      slug =
        config[:slug] || path |> Path.basename() |> Path.rootname() |> String.replace("_", "-")

      %{
        ref: config[:ref],
        slug: slug,
        livemd: markdown,
        title: notebook.name,
        files: files,
        details:
          if config_details = config[:details] do
            description =
              config_details[:description] ||
                raise "missing required :description attribute in notebook details: #{inspect(config_details)}"

            cover =
              case config_details do
                %{cover_filename: filename} ->
                  {:static, filename}

                %{cover_path: path} ->
                  {:url, Livebook.Utils.read_as_data_url!(path)}

                _ ->
                  raise "expected either :cover_path or :cover_filename in notebooks details: #{inspect(config_details)}"
              end

            %{description: description, cover: cover}
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
  Finds learn notebook by slug and returns the parsed data structure.

  Returns the notebook along with the files it uses as preloaded binaries.
  """
  @spec notebook_by_slug!(String.t()) :: {Livebook.Notebook.t(), files()}
  def notebook_by_slug!(slug) do
    notebook_infos()
    |> Enum.find(&(&1.slug == slug))
    |> case do
      nil ->
        raise NotFoundError, slug: slug

      notebook_info ->
        {notebook, []} = Livebook.LiveMarkdown.notebook_from_livemd(notebook_info.livemd)

        {notebook, notebook_info.files}
    end
  end

  @group_configs [
    %{
      title: "Deep dive into Kino",
      description:
        "Learn more about the Kino package, including the creation of custom UI components.",
      cover_filename: "kino.png",
      notebook_refs: [
        :kino_intro,
        :kino_vm_introspection,
        :kino_custom_kinos,
        :kino_smart_cells
      ]
    }
  ]

  @doc """
  Returns a list of all defined notebook groups.
  """
  @spec group_infos() :: list(group_info())
  def group_infos() do
    notebook_infos = notebook_infos()

    for config <- @group_configs do
      %{
        title: config.title,
        description: config.description,
        cover: {:static, config.cover_filename},
        notebook_infos:
          for(
            ref <- config.notebook_refs,
            info = Enum.find(notebook_infos, &(&1[:ref] == ref)),
            do: info
          )
      }
    end
  end
end
