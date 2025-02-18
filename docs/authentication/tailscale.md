# Tailscale

Setting up Tailscale authentication will protect all routes of your Livebook instance. It is particularly useful for adding authentication to Livebook instances with deployed notebooks. Tailscale authentication occurs in addition to [Livebook's authentication](../authentication.md) for deployed notebooks and admins.

Once Tailscale is enabled, we recommend leaving the "/public" route of your instances still public. This route is used for integration with the [Livebook Badge](https://livebook.dev/badge/) and other conveniences.

## How to

To integrate Tailscale authentication with Livebook,
set the `LIVEBOOK_IDENTITY_PROVIDER` environment variable to `tailscale:tailscale-socket-path`, make sure the `tailscale` CLI is installed and available on your machine (or your Docker image).

If you want to access Livebook on the same machine as you are hosting it,
you must also set the `LIVEBOOK_IP` variable to your Tailscale IP.

To do both of these things, run:

```bash
LIVEBOOK_IP=$(tailscale ip -1 | tr -d '\n') \
LIVEBOOK_IDENTITY_PROVIDER=tailscale:/var/run/tailscale/tailscaled.sock \
livebook server
```

See https://tailscale.com/blog/tailscale-auth-nginx/ for more information
on how Tailscale authentication works.

### macOS

On macOS, Tailscale uses a password-protected TCP port instead of a unix socket. The following script automatically detects the port and password, then starts Livebook with the correct Tailscale configuration:

```bash
#!/bin/bash
# This script is adapted from https://github.com/tailscale/tailscale/blob/v1.80.2/safesocket/safesocket_darwin.go#L69-L160

# When Tailscale was installed via Mac App Store
port_and_token=$(lsof -n -a -c IPNExtension -F | grep -o "sameuserproof-[0-9]*-[a-f0-9]*" | head -1)
if [ ! -z "$port_and_token" ]; then
    port=$(echo "$port_and_token" | cut -d'-' -f2)
    token=$(echo "$port_and_token" | cut -d'-' -f3)
else
    # When Tailscale was installed using the standalone variant
    port=$(readlink /Library/Tailscale/ipnport)
    if [ ! -z "$port" ]; then
        token=$(cat "/Library/Tailscale/sameuserproof-$port")
    fi
fi

tailscale_ip=$(exec $(ps -xo comm | grep MacOS/Tailscale$) ip | head -1 | tr -d '\n')

if [ ! -z "$port" ] && [ ! -z "$token" ] && [ ! -z "$tailscale_ip" ]; then
    LIVEBOOK_IP=$tailscale_ip \
    LIVEBOOK_IDENTITY_PROVIDER=tailscale:http://:$token@127.0.0.1:$port \
    livebook server
else
    echo "Error: Missing required configuration"
    [ -z "$port" ] && echo "- Could not determine port"
    [ -z "$token" ] && echo "- Could not determine token"
    [ -z "$tailscale_ip" ] && echo "- Could not determine Tailscale IP"
fi
```

## Livebook Teams

[Livebook Teams](https://livebook.dev/teams/) users can deploy notebooks with the click of a button with built-in authentication via Livebook Teams. You can also pre-configure environment variables (such as `LIVEBOOK_IDENTITY_PROVIDER`), share team secrets, and file storages. Both online and airgapped deployment mechanisms are supported.

Furthermore, if you are deploying multi-session apps via [Livebook Teams](https://livebook.dev/teams/), you can programmatically access data from the authenticated user by calling [`Kino.Workspace.app_info/0`](https://hexdocs.pm/kino/Kino.Workspace.html#app_info/0).

To get started, open up Livebook, click "Add Organization" on the sidebar. Then, inside the notebook of your choice, click "Deploy with Livebook Teams".
