defmodule Livebook.ZTA.Tailscale do
  @moduledoc """
  To integrate Tailscale authentication with Livebook,
  set the `LIVEBOOK_IDENTITY_PROVIDER` environment variable to `tailscale:tailscale-socket-path`.

  If you want to access the Livebook on the same machine as you are hosting it on,
  you will also need to set the LIVEBOOK_IP variable to your Tailscale IP.

  To do both of these things, run

  ```bash
  LIVEBOOK_IP=$(tailscale ip -1 | tr -d '\n') LIVEBOOK_IDENTITY_PROVIDER=tailscale:/var/run/tailscale/tailscaled.sock livebook server
  ```

  See https://tailscale.com/blog/tailscale-auth-nginx/ for more information
  on how Tailscale authorization works.

  ## MacOS

  On MacOS, when Tailscale is installed via the Mac App Store, no unix socket is exposed.
  Instead, a TCP port is made available, protected via a password, which needs to be located.
  Tailscale itself uses lsof for this. This method is replicated in the bash script below,
  which will start Livebook with your Tailscale IP and correct port and password.

  ```bash
  #!/bin/bash
  addr_info=$(lsof -n -a -c IPNExtension -F | sed -n 's/.*sameuserproof-\([[:digit:]]*-.*\).*/\1/p')
  port=$(echo "$addr_info" | cut -d '-' -f 1)
  pass=$(echo "$addr_info" | cut -d '-' -f 2)
  LIVEBOOK_IP=$(exec $(ps -xo comm | grep MacOS/Tailscale$) ip | head -1 | tr -d '\n') LIVEBOOK_IDENTITY_PROVIDER=tailscale:http://:$pass@127.0.0.1:$port livebook server
  ```
  """

  use GenServer
  require Logger

  defstruct [:name, :address]

  def start_link(opts) do
    options = [address: opts[:identity][:key]]
    GenServer.start_link(__MODULE__, options, name: opts[:name])
  end

  def authenticate(name, conn, _) do
    remote_ip = to_string(:inet_parse.ntoa(conn.remote_ip))
    tailscale_address = GenServer.call(name, :get_address)
    user = authenticate_ip(remote_ip, tailscale_address)
    {conn, user}
  end

  @impl true
  def init(options) do
    state = struct!(__MODULE__, options)
    {:ok, state}
  end

  @impl true
  def handle_call(:get_address, _from, state) do
    {:reply, state.address, state}
  end

  defp authenticate_ip(remote_ip, address) do
    {url, options} =
      if String.starts_with?(address, "http") do
        uri = URI.parse(address)

        options =
          if uri.userinfo do
            # Req does not handle userinfo as part of the URL
            [auth: "Basic #{Base.encode64(uri.userinfo)}"]
          else
            []
          end

        url = to_string(%{uri | userinfo: nil, path: "/localapi/v0/whois?addr=#{remote_ip}:1"})

        {url, options}
      else
        # Assume address not starting with http is a Unix socket
        unless File.exists?(address) do
          raise "Tailscale socket does not exist: #{inspect(address)}"
        end

        {
          "http://local-tailscaled.sock/localapi/v0/whois?addr=#{remote_ip}:1",
          [
            unix_socket: address,
            # Req or Finch do not pass on the host from the URL when using a unix socket,
            # so we set the host header explicitly
            headers: [host: "local-tailscaled.sock"]
          ]
        }
      end

    with {:ok, response} <- Req.get(url, options),
         200 <- response.status,
         %{"UserProfile" => user} <- response.body do
      %{
        id: to_string(user["ID"]),
        name: user["DisplayName"],
        email: user["LoginName"]
      }
    else
      _ -> nil
    end
  end
end
