defmodule Livebook.K8sAPI do
  # Calls to the Kubernetes API.

  @type kubeconfig :: Kubereq.Kubeconfig.t()
  @type error :: %{message: String.t(), status: pos_integer() | nil}

  @doc """
  Creates a new Pod.
  """
  @spec create_pod(kubeconfig(), map()) :: {:ok, data} | error()
        when data: %{name: String.t()}
  def create_pod(kubeconfig, manifest) do
    req = pod_req(kubeconfig)

    case Kubereq.create(req, manifest) do
      {:ok, %{status: 201, body: %{"metadata" => %{"name" => name}}}} ->
        {:ok, %{name: name}}

      other ->
        result_to_error(other)
    end
  end

  @doc """
  Fetches Pod with the given name.
  """
  @spec get_pod(kubeconfig(), String.t(), String.t()) :: {:ok, data} | error()
        when data: %{ip: String.t()}
  def get_pod(kubeconfig, namespace, name) do
    req = pod_req(kubeconfig)

    case Kubereq.get(req, namespace, name) do
      {:ok, %{status: 200, body: pod}} ->
        {:ok, %{ip: pod["status"]["podIP"]}}

      other ->
        result_to_error(other)
    end
  end

  @doc """
  Deletes Pod with the given name.
  """
  @spec delete_pod(kubeconfig(), String.t(), String.t()) :: :ok | error()
  def delete_pod(kubeconfig, namespace, name) do
    req = pod_req(kubeconfig)

    case Kubereq.delete(req, namespace, name) do
      {:ok, %{status: 200}} -> :ok
      other -> result_to_error(other)
    end
  end

  @doc """
  Awaits Pod to reach the ready status.
  """
  @spec await_pod_ready(kubeconfig(), String.t(), String.t()) :: :ok | error()
  def await_pod_ready(kubeconfig, namespace, name) do
    req = pod_req(kubeconfig)

    callback = fn
      :deleted ->
        {:error, %{message: "the Pod has been deleted", status: nil}}

      pod ->
        get_in(pod, [
          "status",
          "conditions",
          Access.filter(&(&1["type"] == "Ready")),
          "status"
        ]) == ["True"]
    end

    # Wait up to 30 minutes
    case Kubereq.wait_until(req, namespace, name, callback, timeout: 1_800_000) do
      {:error, :watch_timeout} ->
        {:error, %{message: "timed out waiting for Pod to become ready", status: nil}}

      other ->
        other
    end
  end

  @doc """
  Watches and streams Pod events to the caller.

  The emitted events have the following shape:

      %{message: String.t()}

  """
  @spec watch_pod_events(kubeconfig(), String.t(), String.t()) :: {:ok, Enumerable.t()} | error()
  def watch_pod_events(kubeconfig, namespace, name) do
    req = build_req() |> Kubereq.attach(kubeconfig: kubeconfig, api_version: "v1", kind: "Event")

    Kubereq.watch(req, namespace,
      field_selectors: [
        {"involvedObject.kind", "Pod"},
        {"involvedObject.name", name}
      ]
    )
    |> case do
      {:ok, stream} ->
        stream =
          Stream.map(stream, fn event ->
            %{message: event["object"]["message"]}
          end)

        {:ok, stream}

      other ->
        result_to_error(other)
    end
  end

  @doc """
  Lists all namespaces in the cluster.
  """
  @spec list_namespaces(kubeconfig()) :: {:ok, data} | error()
        when data: list(%{name: String.t()})
  def list_namespaces(kubeconfig) do
    req =
      build_req() |> Kubereq.attach(kubeconfig: kubeconfig, api_version: "v1", kind: "Namespace")

    case Kubereq.list(req, nil) do
      {:ok, %{status: 200, body: %{"items" => items}}} ->
        namespaces =
          for item <- items do
            %{name: item["metadata"]["name"]}
          end

        {:ok, namespaces}

      other ->
        result_to_error(other)
    end
  end

  @doc """
  Creates a Persistent Volume Claim.
  """
  @spec create_pvc(kubeconfig(), map()) :: {:ok, data} | error()
        when data: %{name: String.t()}
  def create_pvc(kubeconfig, manifest) do
    req = pvc_req(kubeconfig)

    case Kubereq.create(req, manifest) do
      {:ok, %{status: 201, body: %{"metadata" => %{"name" => name}}}} ->
        {:ok, %{name: name}}

      other ->
        result_to_error(other)
    end
  end

  @doc """
  Lists all Persistent Volume Claims.
  """
  @spec list_pvcs(kubeconfig(), String.t()) :: {:ok, data} | error()
        when data: list(%{name: String.t(), deleted_at: String.t() | nil})
  def list_pvcs(kubeconfig, namespace) do
    req = pvc_req(kubeconfig)

    case Kubereq.list(req, namespace) do
      {:ok, %{status: 200, body: %{"items" => items}}} ->
        storage_classes =
          for item <- items do
            %{
              name: item["metadata"]["name"],
              deleted_at: item["metadata"]["deletionTimestamp"]
            }
          end

        {:ok, storage_classes}

      other ->
        result_to_error(other)
    end
  end

  @doc """
  Deletes Persistent Volume Claim with the given name.
  """
  @spec delete_pvc(kubeconfig(), String.t(), String.t()) :: :ok | error()
  def delete_pvc(kubeconfig, namespace, name) do
    req = pvc_req(kubeconfig)

    case Kubereq.delete(req, namespace, name) do
      {:ok, %{status: 200}} -> :ok
      other -> result_to_error(other)
    end
  end

  @doc """
  Lists storage classes available in the cluster.
  """
  @spec list_storage_classes(kubeconfig()) :: {:ok, data} | error()
        when data: list(%{name: String.t()})
  def list_storage_classes(kubeconfig) do
    req =
      build_req()
      |> Kubereq.attach(
        kubeconfig: kubeconfig,
        api_version: "storage.k8s.io/v1",
        kind: "StorageClass"
      )

    case Kubereq.list(req, nil) do
      {:ok, %{status: 200, body: %{"items" => items}}} ->
        storage_classes =
          for item <- items do
            %{name: item["metadata"]["name"]}
          end

        {:ok, storage_classes}

      other ->
        result_to_error(other)
    end
  end

  @doc """
  Reviews access according to `resource_attributes`.

  Implements Access Review checks for the authenticated user using
  the `SelfSubjectAccessReview` [1] resource.

  [1]: https://kubernetes.io/docs/reference/kubernetes-api/authorization-resources/self-subject-access-review-v1/#SelfSubjectAccessReviewSpec
  """
  @spec create_access_review(kubeconfig(), keyword()) :: {:ok, data} | error()
        when data: %{
               allowed: boolean(),
               resource: String.t(),
               verb: String.t(),
               namespace: String.t() | nil,
               group: String.t(),
               version: String.t()
             }
  def create_access_review(kubeconfig, resource_attributes) do
    resource_attributes =
      resource_attributes
      |> Keyword.validate!([
        :name,
        :namespace,
        :path,
        :resource,
        :subresource,
        :verb,
        :version,
        group: ""
      ])
      |> Enum.into(%{})

    access_review = %{
      "apiVersion" => "authorization.k8s.io/v1",
      "kind" => "SelfSubjectAccessReview",
      "spec" => %{
        "resourceAttributes" => resource_attributes
      }
    }

    req =
      build_req()
      |> Kubereq.attach(
        kubeconfig: kubeconfig,
        api_version: "authorization.k8s.io/v1",
        kind: "SelfSubjectAccessReview"
      )

    case Kubereq.create(req, access_review) do
      {:ok, %Req.Response{status: 201, body: body}} ->
        resource_attributes = body["spec"]["resourceAttributes"]

        {:ok,
         %{
           allowed: body["status"]["allowed"],
           resource: resource_attributes["resource"],
           verb: resource_attributes["verb"],
           namespace: resource_attributes["namespace"],
           group: resource_attributes["group"],
           version: resource_attributes["version"]
         }}

      other ->
        result_to_error(other)
    end
  end

  defp result_to_error({:ok, %{status: status, body: body}}) do
    message =
      case body do
        %{"message" => message} when is_binary(message) ->
          message

        _ ->
          "HTTP status #{status}"
      end

    {:error, %{message: message, status: status}}
  end

  defp result_to_error({:error, exception}) do
    {:error, %{message: "reason: #{Exception.message(exception)}", status: nil}}
  end

  defp pod_req(kubeconfig) do
    build_req() |> Kubereq.attach(kubeconfig: kubeconfig, api_version: "v1", kind: "Pod")
  end

  defp pvc_req(kubeconfig) do
    build_req()
    |> Kubereq.attach(kubeconfig: kubeconfig, api_version: "v1", kind: "PersistentVolumeClaim")
  end

  defp build_req() do
    Req.new() |> Livebook.Utils.req_attach_defaults()
  end
end
