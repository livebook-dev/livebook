defmodule Livebook do
  @moduledoc """
  Livebook is an interactive notebook system for Elixir.

  This module includes the public API.
  """

  @doc """
  Executes Livebook's config/runtime.exs.

  If you use Livebook as a dependency, you can add the following
  to your `config/runtime.exs` to trigger Livebook's config/runtime.exs
  configuration:

      Livebook.config_runtime()

  """
  def config_runtime do
    import Config

    config :livebook, LivebookWeb.Endpoint,
      secret_key_base:
        Livebook.Config.secret!("LIVEBOOK_SECRET_KEY_BASE") ||
          Base.encode64(:crypto.strong_rand_bytes(48))

    if port = Livebook.Config.port!("LIVEBOOK_PORT") do
      config :livebook, LivebookWeb.Endpoint, http: [port: port]
    end

    if ip = Livebook.Config.ip!("LIVEBOOK_IP") do
      config :livebook, LivebookWeb.Endpoint, http: [ip: ip]
    end

    cond do
      password = Livebook.Config.password!("LIVEBOOK_PASSWORD") ->
        config :livebook, authentication_mode: :password, password: password

      Livebook.Config.token_enabled!("LIVEBOOK_TOKEN_ENABLED") ->
        config :livebook, token: Livebook.Utils.random_id()

      true ->
        config :livebook, authentication_mode: :disabled
    end

    if runtime = Livebook.Config.default_runtime!("LIVEBOOK_DEFAULT_RUNTIME") do
      config :livebook, :default_runtime, runtime
    end

    config :livebook,
           :cookie,
           Livebook.Config.cookie!("LIVEBOOK_COOKIE") ||
             Livebook.Config.cookie!("RELEASE_COOKIE") ||
             Livebook.Utils.random_cookie()

    root_path =
      Livebook.Config.root_path!("LIVEBOOK_ROOT_PATH")
      |> Livebook.FileSystem.Utils.ensure_dir_path()

    local_file_system = Livebook.FileSystem.Local.new(default_path: root_path)
    configured_file_systems = Livebook.Config.file_systems!("LIVEBOOK_FILE_SYSTEM_")

    config :livebook, :file_systems, [local_file_system | configured_file_systems]

    autosave_path =
      if config_env() == :test do
        nil
      else
        Livebook.Config.autosave_path!("LIVEBOOK_AUTOSAVE_PATH")
      end

    config :livebook, :autosave_path, autosave_path
  end

  @doc """
  Parses the given Live Markdown document and converts it to Elixir
  source code.

  ## Limitations

  Note that the resulting script may not compile in some cases, for
  example if you define a macro in one cell and import it in another
  cell, it works fine in Livebook, because each cell is compiled
  separately. However, when running the script it gets compiled as a
  whole and consequently doing so doesn't work.

  Additionally, branching sections are commented out.
  """
  @spec live_markdown_to_elixir(String.t()) :: String.t()
  def live_markdown_to_elixir(markdown) do
    {notebook, _messages} = Livebook.LiveMarkdown.Import.notebook_from_markdown(markdown)
    Livebook.Notebook.Export.Elixir.notebook_to_elixir(notebook)
  end
end
