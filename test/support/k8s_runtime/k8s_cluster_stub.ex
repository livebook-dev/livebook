defmodule Livebook.K8sClusterStub do
  use Plug.Router

  require Logger

  plug :match

  plug Plug.Parsers,
    parsers: [:urlencoded, :json],
    json_decoder: Jason

  plug :dispatch

  post "/apis/authorization.k8s.io/v1/selfsubjectaccessreviews" do
    conn
    |> put_status(201)
    |> Req.Test.json(%{"status" => %{"allowed" => true}})
  end

  get "/api/v1/namespaces" do
    Req.Test.json(conn, %{"items" => [%{"metadata" => %{"name" => "default"}}]})
  end

  get "apis/storage.k8s.io/v1/storageclasses" do
    Req.Test.json(conn, %{
      "items" => [
        %{"metadata" => %{"name" => "first-storage-class"}},
        %{"metadata" => %{"name" => "second-storage-class"}}
      ]
    })
  end

  get "/api/v1/namespaces/default/persistentvolumeclaims" do
    Req.Test.json(conn, %{
      "items" => [
        %{"metadata" => %{"name" => "foo-pvc"}},
        %{"metadata" => %{"name" => "new-pvc"}}
      ]
    })
  end

  delete "/api/v1/namespaces/default/persistentvolumeclaims/:name" do
    send_resp(conn, 200, "")
  end

  post "/api/v1/namespaces/default/persistentvolumeclaims" do
    conn
    |> put_status(201)
    |> Req.Test.json(%{"metadata" => %{"name" => "new-pvc"}})
  end

  match _ do
    Logger.error("Unimplemented #{conn.method} Stub Request to #{conn.request_path}")

    conn
    |> put_status(500)
    |> Req.Test.text("Endpoint not implemented")
  end
end
