defmodule Livebook.K8s.Auth do
  @moduledoc """
  Implementation of Access Review checks for the authenticated user using the
  [`SelfSubjectAccessReview`](https://kubernetes.io/docs/reference/kubernetes-api/authorization-resources/self-subject-access-review-v1/#SelfSubjectAccessReviewSpec)
  resource
  """

  @doc """
  Concurrently reviews access according to a list of `resource_attributes`.
  Expects `req` to be prepared for `SelfSubjectAccessReview`.
  """
  @spec batch_check(Req.Request.t(), [keyword()]) ::
          [:ok | {:error, %Req.Response{}} | {:error, Exception.t()}]
  def batch_check(req, resource_attribute_list) do
    resource_attribute_list
    |> Enum.map(&Task.async(fn -> can_i?(req, &1) end))
    # 30 second timeout
    |> Task.await_many(30_000)
  end

  @doc """
  Reviews access according to `resource_attributes`.
  Expects `req` to be prepared for `SelfSubjectAccessReview`.
  """
  @spec can_i?(Req.Request.t(), keyword()) ::
          :ok | {:error, %Req.Response{}} | {:error, Exception.t()}
  def can_i?(req, resource_attributes) do
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

    create_self_subject_access_review(req, access_review)
  end

  defp create_self_subject_access_review(req, access_review) do
    case Kubereq.create(req, access_review) do
      {:ok, %Req.Response{status: 201, body: %{"status" => %{"allowed" => true}}}} ->
        :ok

      {:ok, %Req.Response{} = response} ->
        {:error, response}

      {:error, error} ->
        {:error, error}
    end
  end
end
