# Custom

It is possible to provide custom Zero Trust Authentication (ZTA) inside Livebook's Docker images.

To do so, you must define a file with the `.exs` extension inside the `/app/user/extensions` of your Livebook image, for example, `/app/user/extensions/my_auth.exs`. This file should define at least one module, which implements the ZTA skeleton below:

```elixir
defmodule MyAuth do
  use GenServer

  @spec start_link(keyword) :: {:ok, pid()}
  def start_link(opts) do
    identity_key = opts[:identity_key]
    GenServer.start_link(__MODULE__, identity_key, Keyword.take(opts, [:name]))
  end

  @spec authenticate(GenServer.server(), Plug.Conn.t(), keyword()) ::
          {Plug.Conn.t(), map() | nil}
  def authenticate(server, conn, opts \\ []) do
    # Connects to the GenServer given by `server` and returns the user information.
    # See `opts[:fields]` for the fields to be returned in the map.
    # Return nil if the user cannot be authenticated.
  end
end
```

Then you must configure Livebook to use the module above as your identity provider:

```bash
LIVEBOOK_IDENTITY_PROVIDER="custom:MyAuth"
```

Or, if you want to pass a custom identity key:

```bash
LIVEBOOK_IDENTITY_PROVIDER="custom:MyAuth:my-key"
```

Keep in mind that the identity provider contract in Livebook is still evolving and it may change in future releases. Additionally, your code may rely on two dependencies: [Req ~> 0.4](https://hexdocs.pm/req) and [JOSE ~> 1.11](https://hexdocs.pm/jose).

## Development

If you want to try your custom identity provider in development, you can [clone Livebook's git repository](https://github.com/livebook-dev/livebook) and then execute the following command inside Livebook's root folder:

```elixir
$ mix setup
$ LIVEBOOK_IDENTITY_PROVIDER="custom:MyAuth" elixir -r path/to/my_auth.exs -S mix phx.server
```