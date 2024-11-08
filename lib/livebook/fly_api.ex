defmodule Livebook.FlyAPI do
  # Calls to the Fly API.
  #
  # Note that Fly currently exposes both a REST Machines API [1] and
  # a more elaborate GraphQL API [2]. The Machines API should be
  # preferred whenever possible. The Go client [3] serves as a good
  # reference for various operations.
  #
  # [1]: https://fly.io/docs/machines/api
  # [2]: https://github.com/superfly/fly-go/blob/v0.1.18/schema.graphql
  # [3]: https://github.com/superfly/fly-go

  # See https://github.com/superfly/fly-go/blob/ea7601fc38ba5e9786155711471646dcb0bf63b8/flaps/flaps_volumes.go#L12
  @destroyed_volume_states ~w(scheduling_destroy fork_cleanup waiting_for_detach pending_destroy destroying)

  @api_url "https://api.fly.io/graphql"
  @flaps_url "https://api.machines.dev"

  @type error :: %{message: String.t(), status: pos_integer() | nil}

  @doc """
  The valid values for CPU kind.
  """
  @spec cpu_kinds() :: list(String.t())
  def cpu_kinds(), do: ~w(shared performance)

  @doc """
  The valid values for GPU kind.
  """
  @spec gpu_kinds() :: list(String.t())
  def gpu_kinds(), do: ~w(a10 a100-pcie-40gb a100-sxm4-80gb l40s)

  @doc """
  Fetches information about organizations visible to the given token
  and also regions data.
  """
  @spec get_orgs_and_regions(String.t()) :: {:ok, data} | {:error, error}
        when data: %{
               orgs: list(%{name: String.t(), slug: String.t()}),
               regions: %{name: String.t(), code: String.t()},
               closest_region: String.t()
             }
  def get_orgs_and_regions(token) do
    query = """
    query {
      organizations {
        nodes {
          rawSlug
          name
        }
      }
      platform {
        requestRegion
        regions {
          name
          code
        }
      }
    }
    """

    with {:ok, data} <- api_request(token, query) do
      {:ok,
       %{
         orgs: Enum.map(data["organizations"]["nodes"], &parse_org/1),
         regions: Enum.map(data["platform"]["regions"], &parse_region/1),
         closest_region: data["platform"]["requestRegion"]
       }}
    end
  end

  defp parse_org(org) do
    %{name: org["name"], slug: org["rawSlug"]}
  end

  defp parse_region(region) do
    %{code: region["code"], name: region["name"]}
  end

  @doc """
  Fetches volumes in the given app.

  Note that destroyed volumes are ignored.
  """
  @spec get_app_volumes(String.t(), String.t()) :: {:ok, data} | {:error, error}
        when data:
               list(%{
                 id: String.t(),
                 name: String.t(),
                 region: String.t(),
                 size_gb: pos_integer()
               })
  def get_app_volumes(token, app_name) do
    with {:ok, data} <- flaps_request(token, "/v1/apps/#{app_name}/volumes") do
      volumes =
        for volume <- data,
            volume["state"] not in @destroyed_volume_states,
            do: parse_volume(volume)

      {:ok, volumes}
    end
  end

  defp parse_volume(volume) do
    %{
      id: volume["id"],
      name: volume["name"],
      region: volume["region"],
      size_gb: volume["size_gb"]
    }
  end

  @doc """
  Creates an app under the given organization.
  """
  @spec create_app(String.t(), String.t(), String.t()) :: :ok | {:error, error}
  def create_app(token, app_name, org_slug) do
    with {:ok, _data} <-
           flaps_request(token, "/v1/apps",
             method: :post,
             json: %{app_name: app_name, org_slug: org_slug}
           ) do
      :ok
    end
  end

  @doc """
  Creates a new volume in the given app.

  The `compute` attributes hint the expected machine specs that this
  volume will be attached to. This helps to ensure that the volume is
  placed on the right hardware (e.g. GPU-enabled).
  """
  @spec create_volume(String.t(), String.t(), String.t(), String.t(), pos_integer(), map()) ::
          {:ok, data} | {:error, error}
        when data: %{
               id: String.t(),
               name: String.t(),
               region: String.t(),
               size_gb: pos_integer()
             }
  def create_volume(token, app_name, name, region, size_gb, compute) do
    with {:ok, data} <-
           flaps_request(token, "/v1/apps/#{app_name}/volumes",
             method: :post,
             json: %{
               name: name,
               size_gb: size_gb,
               region: region,
               compute: compute
             }
           ) do
      {:ok, parse_volume(data)}
    end
  end

  @doc """
  Deletes the given volume.
  """
  @spec delete_volume(String.t(), String.t(), String.t()) :: :ok | {:error, error}
  def delete_volume(token, app_name, volume_id) do
    with {:ok, _data} <-
           flaps_request(token, "/v1/apps/#{app_name}/volumes/#{volume_id}", method: :delete) do
      :ok
    end
  end

  @doc """
  Creates a new machine in the given app.
  """
  @spec create_machine(String.t(), String.t(), String.t(), String.t(), map()) ::
          {:ok, data} | {:error, error}
        when data: %{id: String.t(), private_ip: String.t()}
  def create_machine(token, app_name, name, region, config) do
    boot_timeout = 30_000

    with {:ok, data} <-
           flaps_request(token, "/v1/apps/#{app_name}/machines",
             method: :post,
             json: %{name: name, region: region, config: config},
             receive_timeout: boot_timeout
           ) do
      {:ok, parse_machine(data)}
    end
  end

  defp parse_machine(machine) do
    %{id: machine["id"], private_ip: machine["private_ip"]}
  end

  @doc """
  Deletes the given machine.
  """
  @spec delete_machine(String.t(), String.t(), String.t()) :: :ok | {:error, error}
  def delete_machine(token, app_name, machine_id) do
    params = %{force: true}

    with {:ok, _data} <-
           flaps_request(token, "/v1/apps/#{app_name}/machines/#{machine_id}",
             method: :delete,
             params: params
           ) do
      :ok
    end
  end

  @doc """
  Waits for the machine to start.
  """
  @spec await_machine_started(String.t(), String.t(), String.t()) :: :ok | {:error, error}
  def await_machine_started(token, app_name, machine_id) do
    # The maximum supported timeout is 60s, but the machine may take
    # longer to start if it uses a large Docker image (such as CUDA),
    # provided the image is not already in the Fly cache. To achieve
    # a longer wait, we retry request timeouts (and possible network
    # errors).
    with {:ok, _data} <-
           flaps_request(token, "/v1/apps/#{app_name}/machines/#{machine_id}/wait",
             params: %{state: "started", timeout: 60},
             receive_timeout: 90_000,
             retry: :safe_transient,
             max_retries: 4,
             retry_log_level: false
           ) do
      :ok
    end
  end

  @doc """
  Waits for the machine to be destroyed.
  """
  @spec await_machine_destroyed(String.t(), String.t(), String.t(), pos_integer()) ::
          :ok | {:error, error}
  def await_machine_destroyed(token, app_name, machine_id, timeout_s) when timeout_s <= 60 do
    # Contrarily to the above, if we expect the machine to be destroying,
    # it should take a short time, so we don't retry requests and expect
    # a rather short timeout
    with {:ok, _data} <-
           flaps_request(token, "/v1/apps/#{app_name}/machines/#{machine_id}/wait",
             params: %{state: "destroyed", timeout: timeout_s},
             retry: false
           ) do
      :ok
    end
  end

  defp flaps_request(token, path, opts \\ []) do
    opts =
      [base_url: @flaps_url, url: path, auth: {:bearer, token}]
      |> Keyword.merge(opts)
      |> Keyword.merge(test_options())

    req = opts |> Req.new() |> Livebook.Utils.req_attach_defaults()

    case Req.request(req) do
      {:ok, %{status: status, body: body}} when status in 200..299 ->
        {:ok, body}

      {:ok, %{status: status, body: body}} ->
        message =
          case body do
            %{"error" => error} when is_binary(error) ->
              Livebook.Utils.downcase_first(error)

            _ ->
              "HTTP status #{status}"
          end

        {:error, %{message: message, status: status}}

      {:error, exception} ->
        {:error, %{message: "reason: #{Exception.message(exception)}", status: nil}}
    end
  end

  defp api_request(token, query) do
    opts =
      [
        base_url: @api_url,
        method: :post,
        auth: {:bearer, token},
        json: %{query: query}
      ]
      |> Keyword.merge(test_options())

    req = opts |> Req.new() |> Livebook.Utils.req_attach_defaults()

    case Req.request(req) do
      {:ok, %{status: 200, body: body}} ->
        case body do
          %{"errors" => [%{"extensions" => %{"code" => "UNAUTHORIZED"}} | _]} ->
            {:error, %{message: "could not authorize with the given token", status: 401}}

          %{"errors" => [%{"extensions" => %{"message" => message}} | _]} ->
            {:error, %{message: Livebook.Utils.downcase_first(message), status: nil}}

          %{"data" => data} ->
            {:ok, data}
        end

      {:ok, %{status: status}} ->
        {:error, %{message: "HTTP status #{status}", status: status}}

      {:error, exception} ->
        {:error, %{message: "reason: #{Exception.message(exception)}", status: nil}}
    end
  end

  # TODO: do not rely on private APIs. Also, ideally we should still
  # be able to use Req.Test.expect/2
  if Mix.env() == :test do
    defp test_options() do
      case Req.Test.__fetch_plug__(__MODULE__) do
        :passthrough ->
          []

        _plug ->
          [plug: {Req.Test, __MODULE__}]
      end
    end

    @doc false
    def stub(plug) do
      Req.Test.stub(__MODULE__, plug)
    end

    @doc false
    def passthrough() do
      Req.Test.stub(__MODULE__, :passthrough)
    end
  else
    defp test_options(), do: []
  end
end
