defmodule Livebook.NoPermissionClusterStub do
  use Plug.Router

  require Logger

  plug :match

  plug Plug.Parsers,
    parsers: [:urlencoded, :json],
    json_decoder: Jason

  plug :dispatch

  post "/apis/authorization.k8s.io/v1/selfsubjectaccessreviews" do
    allowed =
      case conn.body_params["spec"]["resourceAttributes"]["resource"] do
        "selfsubjectaccessreviews" ->
          # If a connection to the cluster can be established, anybody can create
          # selfsubjectaccessreviews.
          true

        _ ->
          # We're simulating no permissions so any other resource returns false
          false
      end

    conn
    |> put_status(201)
    |> Req.Test.json(%{"status" => %{"allowed" => allowed}})
  end

  get "/api/v1/namespaces" do
    #  No permission to list namespaces
    send_resp(conn, 403, "")
  end

  match _ do
    Logger.error("Unimplemented #{conn.method} Stub Request to #{conn.request_path}")

    conn
    |> put_status(500)
    |> Req.Test.text("Endpoint not implemented")
  end
end
