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

On macOS, when Tailscale is installed via the Mac App Store, no unix socket is exposed.
Instead, a TCP port is made available and protected via a password, which needs to be located.
Tailscale itself uses lsof for this. This method is replicated in the bash script below,
which will start Livebook with your Tailscale IP and correct port and password.

```bash
#!/bin/bash
addr_info=$(lsof -n -a -c IPNExtension -F | sed -n 's/.*sameuserproof-\([[:digit:]]*-.*\).*/\1/p')
port=$(echo "$addr_info" | cut -d '-' -f 1)
pass=$(echo "$addr_info" | cut -d '-' -f 2)
LIVEBOOK_IP=$(exec $(ps -xo comm | grep MacOS/Tailscale$) ip | head -1 | tr -d '\n') \
LIVEBOOK_IDENTITY_PROVIDER=tailscale:http://:$pass@127.0.0.1:$port \
livebook server
```

## Livebook Teams

[Livebook Teams](https://livebook.dev/teams/) users can deploy notebooks with the click of a button with pre-configured Zero Trust Authentication, shared team secrets, and file storages. Both online and airgapped deployment mechanisms are supported.

Furthermore, if you are deploying multi-session apps via [Livebook Teams](https://livebook.dev/teams/), you can programmatically access data from the authenticated user by calling [`Kino.Workspace.app_info/0`](https://hexdocs.pm/kino/Kino.Workspace.html#app_info/0).

To get started, open up Livebook, click "Add Organization" on the sidebar. Then, inside the notebook of your choice, click "Deploy with Livebook Teams".
