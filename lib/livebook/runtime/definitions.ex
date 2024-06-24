defmodule Livebook.Runtime.Definitions do
  @kino_requirement "~> 0.13.0"

  def kino_requirement do
    @kino_requirement
  end

  kino = %{
    name: "kino",
    dependency: %{dep: {:kino, @kino_requirement}, config: []}
  }

  kino_vega_lite = %{
    name: "kino_vega_lite",
    dependency: %{dep: {:kino_vega_lite, "~> 0.1.11"}, config: []}
  }

  kino_db = %{
    name: "kino_db",
    dependency: %{dep: {:kino_db, "~> 0.2.8"}, config: []}
  }

  exqlite = %{
    name: "exqlite",
    dependency: %{dep: {:exqlite, "~> 0.23.0"}, config: []}
  }

  kino_maplibre = %{
    name: "kino_maplibre",
    dependency: %{dep: {:kino_maplibre, "~> 0.1.12"}, config: []}
  }

  kino_slack = %{
    name: "kino_slack",
    dependency: %{dep: {:kino_slack, "~> 0.1.1"}, config: []}
  }

  kino_bumblebee = %{
    name: "kino_bumblebee",
    dependency: %{dep: {:kino_bumblebee, "~> 0.5.0"}, config: []}
  }

  exla = %{
    name: "exla",
    dependency: %{dep: {:exla, ">= 0.0.0"}, config: [nx: [default_backend: EXLA.Backend]]}
  }

  torchx = %{
    name: "torchx",
    dependency: %{dep: {:torchx, ">= 0.0.0"}, config: [nx: [default_backend: Torchx.Backend]]}
  }

  kino_explorer = %{
    name: "kino_explorer",
    dependency: %{dep: {:kino_explorer, "~> 0.1.20"}, config: []}
  }

  jason = %{
    name: "jason",
    dependency: %{dep: {:jason, "~> 1.4"}, config: []}
  }

  stb_image = %{
    name: "stb_image",
    dependency: %{dep: {:stb_image, "~> 0.6.9"}, config: []}
  }

  xlsx_reader = %{
    name: "xlsx_reader",
    dependency: %{dep: {:xlsx_reader, "~> 0.8.5"}, config: []}
  }

  windows? = match?({:win32, _}, :os.type())
  nx_backend_package = if(windows?, do: torchx, else: exla)

  @smart_cell_definitions [
    %{
      kind: "Elixir.KinoDB.ConnectionCell",
      name: "Database connection",
      requirement_presets: [
        %{
          name: "Amazon Athena",
          packages: [
            kino_db,
            %{
              name: "req_athena",
              dependency: %{dep: {:req_athena, ">= 0.0.0"}, config: []}
            }
          ]
        },
        %{
          name: "Google BigQuery",
          packages: [
            kino_db,
            %{
              name: "req_bigquery",
              dependency: %{dep: {:req_bigquery, ">= 0.0.0"}, config: []}
            }
          ]
        },
        %{
          name: "MySQL",
          packages: [
            kino_db,
            %{name: "myxql", dependency: %{dep: {:myxql, ">= 0.0.0"}, config: []}}
          ]
        },
        %{
          name: "PostgreSQL",
          packages: [
            kino_db,
            %{name: "postgrex", dependency: %{dep: {:postgrex, ">= 0.0.0"}, config: []}}
          ]
        },
        %{
          name: "Snowflake",
          packages: [
            kino_db,
            kino_explorer,
            %{name: "adbc", dependency: %{dep: {:adbc, ">= 0.0.0"}, config: []}}
          ]
        },
        %{
          name: "SQLite",
          packages: [kino_db, exqlite]
        },
        %{
          name: "SQLServer",
          packages: [
            kino_db,
            %{name: "tds", dependency: %{dep: {:tds, ">= 0.0.0"}, config: []}}
          ]
        }
      ]
    },
    %{
      kind: "Elixir.KinoDB.SQLCell",
      name: "SQL query",
      requirement_presets: [
        %{
          name: "Default",
          packages: [kino_db]
        }
      ]
    },
    %{
      kind: "Elixir.KinoVegaLite.ChartCell",
      name: "Chart",
      requirement_presets: [
        %{
          name: "Default",
          packages: [kino_vega_lite]
        }
      ]
    },
    %{
      kind: "Elixir.KinoMapLibre.MapCell",
      name: "Map",
      requirement_presets: [
        %{
          name: "Default",
          packages: [kino_maplibre]
        }
      ]
    },
    %{
      kind: "Elixir.KinoSlack.MessageCell",
      name: "Slack message",
      requirement_presets: [
        %{
          name: "Default",
          packages: [kino_slack]
        }
      ]
    },
    %{
      kind: "Elixir.KinoBumblebee.TaskCell",
      name: "Neural Network task",
      requirement_presets: [
        %{
          name: "Default",
          packages: [kino_bumblebee, nx_backend_package]
        }
      ]
    },
    %{
      kind: "Elixir.KinoExplorer.DataTransformCell",
      name: "Data transform",
      requirement_presets: [
        %{
          name: "Default",
          packages: [kino_explorer]
        }
      ]
    },
    %{
      kind: "Elixir.Kino.RemoteExecutionCell",
      name: "Remote execution",
      requirement_presets: [
        %{
          name: "Default",
          packages: [kino]
        }
      ]
    }
  ]

  @snippet_definitions [
    # Examples
    %{
      type: :example,
      name: "Form",
      icon: "bill-line",
      variants: [
        %{
          name: "Default",
          source: """
          form =
            Kino.Control.form(
              [
                name: Kino.Input.text("Name")
              ],
              submit: "Submit"
            )

          Kino.listen(form, fn event ->
            IO.inspect(event)
          end)

          form\
          """,
          packages: [kino]
        }
      ]
    },
    # File actions
    %{
      type: :file_action,
      file_types: :any,
      description: "Read file content",
      source: """
      content =
        Kino.FS.file_path("{{NAME}}")
        |> File.read!()\
      """,
      packages: [kino]
    },
    %{
      type: :file_action,
      file_types: ["application/json"],
      description: "Parse JSON content",
      source: """
      data =
        Kino.FS.file_path("{{NAME}}")
        |> File.read!()
        |> Jason.decode!()

      Kino.Tree.new(data)\
      """,
      packages: [kino, jason]
    },
    %{
      type: :file_action,
      file_types: ["text/csv"],
      description: "Create a dataframe",
      source: """
      df =
        Kino.FS.file_path("{{NAME}}")
        |> Explorer.DataFrame.from_csv!()\
      """,
      packages: [kino, kino_explorer]
    },
    %{
      type: :file_action,
      file_types: [".parquet"],
      description: "Create a dataframe",
      source: """
      df =
        Kino.FS.file_spec("{{NAME}}")
        |> Explorer.DataFrame.from_parquet!(lazy: true)\
      """,
      packages: [kino, kino_explorer]
    },
    %{
      type: :file_action,
      file_types: ["image/*"],
      description: "Classify image",
      source: """
      # To explore more models, see "+ Smart" > "Neural Network task"

      {:ok, model_info} = Bumblebee.load_model({:hf, "microsoft/resnet-50"})
      {:ok, featurizer} = Bumblebee.load_featurizer({:hf, "microsoft/resnet-50"})

      #{if windows? do
        """
        serving = Bumblebee.Vision.image_classification(model_info, featurizer)\
        """
      else
        """
        serving =
          Bumblebee.Vision.image_classification(model_info, featurizer,
            compile: [batch_size: 1],
            defn_options: [compiler: EXLA]
          )\
        """
      end}

      image = Kino.FS.file_path("{{NAME}}") |> StbImage.read_file!()

      output = Nx.Serving.run(serving, image)\
      """,
      packages: [kino_bumblebee, nx_backend_package, stb_image]
    },
    %{
      type: :file_action,
      file_types: ["image/*"],
      description: "Describe image",
      source: """
      # To explore more models, see "+ Smart" > "Neural Network task"

      repo = {:hf, "Salesforce/blip-image-captioning-base"}
      {:ok, model_info} = Bumblebee.load_model(repo)
      {:ok, featurizer} = Bumblebee.load_featurizer(repo)
      {:ok, tokenizer} = Bumblebee.load_tokenizer(repo)
      {:ok, generation_config} = Bumblebee.load_generation_config(repo)
      generation_config = Bumblebee.configure(generation_config, max_new_tokens: 100)

      #{if windows? do
        """
        serving = Bumblebee.Vision.image_to_text(model_info, featurizer, tokenizer, generation_config)\
        """
      else
        """
        serving =
          Bumblebee.Vision.image_to_text(model_info, featurizer, tokenizer, generation_config,
            compile: [batch_size: 1],
            defn_options: [compiler: EXLA]
          )\
        """
      end}

      image = Kino.FS.file_path("{{NAME}}") |> StbImage.read_file!()

      Nx.Serving.run(serving, image)\
      """,
      packages: [kino_bumblebee, nx_backend_package, stb_image]
    },
    %{
      type: :file_action,
      file_types: ["audio/*"],
      description: "Transcribe speech",
      source:
        """
        # To explore more models, see "+ Smart" > "Neural Network task"

        {:ok, model_info} = Bumblebee.load_model({:hf, "openai/whisper-tiny"})
        {:ok, featurizer} = Bumblebee.load_featurizer({:hf, "openai/whisper-tiny"})
        {:ok, tokenizer} = Bumblebee.load_tokenizer({:hf, "openai/whisper-tiny"})
        {:ok, generation_config} = Bumblebee.load_generation_config({:hf, "openai/whisper-tiny"})
        generation_config = Bumblebee.configure(generation_config, max_new_tokens: 100)

        """ <>
          if windows? do
            """
            serving =
              Bumblebee.Audio.speech_to_text_whisper(model_info, featurizer, tokenizer, generation_config,
                chunk_num_seconds: 30,
                timestamps: :segments,
                stream: true,
                compile: [batch_size: 4]
              )
            """
          else
            """
            serving =
              Bumblebee.Audio.speech_to_text_whisper(model_info, featurizer, tokenizer, generation_config,
                chunk_num_seconds: 30,
                timestamps: :segments,
                stream: true,
                compile: [batch_size: 4],
                defn_options: [compiler: EXLA]
              )
            """
          end <>
          ~S"""

          path = Kino.FS.file_path("{{NAME}}")
          Kino.render(Kino.Text.new("(Start of transcription)", chunk: true))

          for chunk <- Nx.Serving.run(serving, {:file, path}) do
            [start_mark, end_mark] =
              for seconds <- [chunk.start_timestamp_seconds, chunk.end_timestamp_seconds] do
                seconds
                |> round()
                |> Time.from_seconds_after_midnight()
                |> Time.to_string()
              end

            text = "\n#{start_mark}-#{end_mark}: #{chunk.text}"
            Kino.render(Kino.Text.new(text, chunk: true))
          end

          Kino.render(Kino.Text.new("\n(End of transcription)", chunk: true))
          :ok
          """,
      packages: [kino_bumblebee, nx_backend_package]
    },
    %{
      type: :file_action,
      file_types: [".db", ".sqlite"],
      description: "Describe SQLite database",
      source: """
      database_path = Kino.FS.file_path("{{NAME}}")
      {:ok, conn} = Kino.start_child({Exqlite, database: database_path})

      Exqlite.query!(conn, "PRAGMA table_list", [])\
      """,
      packages: [kino_db, exqlite]
    },
    %{
      type: :file_action,
      file_types: [".xlsx", ".xlsm"],
      description: "Read sheets",
      source: """
      xlsx_file = Kino.FS.file_path("{{NAME}}")
      {:ok, package} = XlsxReader.open(xlsx_file)

      tabs =
        for sheet <- XlsxReader.sheet_names(package) do
          maps =
            case XlsxReader.sheet(package, sheet) do
              {:ok, []} ->
                []

              {:ok, [header | rows]} ->
                Enum.map(rows, fn row -> header |> Enum.zip(row) |> Map.new() end)
            end

          {sheet, Kino.DataTable.new(maps)}
        end

      Kino.Layout.tabs(tabs)
      """,
      packages: [kino, xlsx_reader]
    }
  ]

  def smart_cell_definitions(), do: @smart_cell_definitions

  def snippet_definitions(), do: @snippet_definitions
end
