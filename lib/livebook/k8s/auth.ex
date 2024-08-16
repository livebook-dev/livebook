defmodule Livebook.K8s.Auth do
  def create_access_reviews(reqs) do
    can_i?(reqs,
      verb: "create",
      group: "authorization.k8s.io",
      version: "v1",
      resource: "selfsubjectaccessreviews"
    )
  end

  def create_namespaces(reqs) do
    can_i?(reqs, verb: "create", version: "v1", resource: "namespaces")
  end

  def create_pods(reqs, namespace) do
    can_i?(reqs, verb: "create", version: "v1", resource: "pods", namespace: namespace)
  end

  @doc """
  Validates the resource attributes according to the documentation
  https://kubernetes.io/docs/reference/kubernetes-api/authorization-resources/self-subject-access-review-v1/#SelfSubjectAccessReviewSpec
  """
  def can_i?(reqs, resource_attributes) do
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

    create_self_subject_access_review(reqs, access_review)
  end

  defp create_self_subject_access_review(reqs, access_review) do
    case Kubereq.create(reqs.access_reviews, access_review) do
      {:ok, %Req.Response{status: 201, body: %{"status" => %{"allowed" => true}}}} ->
        :ok

      {:ok, %Req.Response{} = response} ->
        {:error, response}

      {:error, error} ->
        {:error, error}
    end
  end
end
