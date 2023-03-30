defmodule LivebookWeb.EndpointTest do
  use LivebookWeb.ConnCase, async: true

  test "delete cookies once they go over a certain limit", %{conn: conn} do
    cookies =
      Enum.map(1..5, &"c#{&1}=#{String.duplicate("a", 4096)}") ++
        Enum.map(1..5, &"lb:#{&1}=#{String.duplicate("a", 4096)}")

    assert [
             "c1=;" <> _,
             "c2=;" <> _,
             "c3=;" <> _,
             "c4=;" <> _,
             "c5=;" <> _,
             "lb:session" <> _,
             "lb:user_data" <> _
           ] =
             conn
             |> put_req_header("cookie", Enum.join(cookies, "; "))
             |> get(~p"/")
             |> get_resp_header("set-cookie")
             |> Enum.sort()
  end
end
