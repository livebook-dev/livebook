defprotocol Livebook.Apps.AppSpec do
  # This protocol defines an interface for an app blueprint.
  #
  # An app spec is used to encapsulate information about how to load
  # the source for an app. The spec is used by `Livebook.Apps.Manager`
  # to deploy permanent apps across the cluster.
  #
  # Every struct implementing this protocol is also expected to have
  # the `:slug` and `:version` (string) attributes.

  @doc """
  Loads the app notebook and other metadata relevant for deployment.

  This function may load the notebook from an external source, so the
  caller should avoid calling it multiple times.

  The function receives a directory that the notebook files should be
  copied into. It is the responsibility of the caller to remove this
  directory, regardless of whether loading succeeds or fails.
  """
  @spec load(t(), String.t()) ::
          {:ok, %{notebook: Livebook.Notebook.t(), warnings: list(String.t())}}
          | {:error, String.t()}
  def load(app_spec, files_tmp_path)

  @doc """
  Returns whether warmup procedure should run, before deploying the
  app.

  You may want to skip warmup, if the app has already been warmed up
  beforehand.
  """
  @spec should_warmup?(t()) :: boolean()
  def should_warmup?(app_spec)
end
