defmodule Livebook.ZTA do
  @moduledoc """
             Enable zero-trust authentication within your Plug/Phoenix application.

             The following ZTA providers are supported:

               * Livebook.ZTA.Cloudflare
               * Livebook.ZTA.GoogleIAP
               * Livebook.ZTA.Tailscale

             We also support the following providers for dev/test/staging:

               * Livebook.ZTA.BasicAuth - HTTP basic authentication with a single user-pass
               * Livebook.ZTA.PassThrough - always succeeds with no metadata

             You can find documentation for setting up these providers under [Livebook's
             authentication section in the sidebar](https://hexdocs.pm/livebook/).

             ## Usage

             First you must add the ZTA provider of your choice to your supervision tree:

                 {Livebook.ZTA.GoogleIAP, name: :google_iap, identity_key: "foobar"}

             where the `identity_key` is the identity provider specific key.

             Then you can use the provider `c:authenticate/3` callback to authenticate
             users on every request:

                 plug :zta

                 def zta(conn, _opts) do
                   case Livebook.ZTA.GoogleIAP.authenticate(conn, :google_iap) do
                     # The provider is redirecting somewhere for follow up
                     {%{halted: true} = conn, nil} ->
                       conn

                     # Authentication failed
                     {%{halted: false} = conn, nil} ->
                       send_resp(conn, 401, "Unauthorized")

                     # Authentication succeeded
                     {conn, metadata} ->
                       put_session(conn, :user_metadata, metadata)
                   end
                 end

             Each provider may have specific optoins supported on `authenticate/3`.
             """ && false

  @type name :: atom()

  @typedoc """
  A metadata of keys returned by zero-trust authentication provider.

  The following keys are supported:

    * `:id` - a string that uniquely identifies the user
    * `:name` - the user name
    * `:email` - the user email
    * `:payload` - the provider payload

  Note that none of the keys are required. The metadata returned depends
  on the provider.
  """
  @type metadata :: %{
          optional(:id) => String.t(),
          optional(:name) => String.t(),
          optional(:email) => String.t(),
          optional(:payload) => map()
        }

  @doc """
  Each provider must specify a child specification for its processes.

  The `:name` and `:identity_key` keys are expected.
  """
  @callback child_spec(name: name(), identity_key: String.t()) :: Supervisor.child_spec()

  @doc """
  Authenticates against the given name.

  It will return one of:

    * `{non_halted_conn, nil}` - the authentication failed and you must
      halt the connection and render the appropriate report

    * `{halted_conn, nil}` - the authentication failed and the connection
      was modified accordingly to request the credentials

    * `{non_halted_conn, metadata}` - the authentication succeed and the
      following metadata about the user is available

  """
  @callback authenticate(name(), Plug.Conn.t(), keyword()) :: {Plug.Conn.t(), metadata() | nil}

  @doc false
  def init do
    :ets.new(__MODULE__, [:named_table, :public, :set, read_concurrency: true])
  end

  def get(name) do
    :ets.lookup_element(__MODULE__, name, 2)
  end

  def put(name, value) do
    :ets.insert(__MODULE__, [{name, value}])
  end
end
