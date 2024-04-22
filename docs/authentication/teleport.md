# Teleport

Setting up Teleport authentication will protect all routes of your Livebook instance. It is particularly useful for adding authentication to Livebook instances with deployed notebooks. Teleport authentication occurs in addition to [Livebook's authentication](../authentication.md) for deployed notebooks and admins.

## How to

To integrate Teleport authentication with Livebook,
set the `LIVEBOOK_IDENTITY_PROVIDER` environment variable to `LIVEBOOK_IDENTITY_PROVIDER=teleport:https://[cluster-name]:3080`.

```bash
LIVEBOOK_IDENTITY_PROVIDER=teleport:https://[cluster-name]:3080 \
livebook server
```

See https://goteleport.com/docs/application-access/jwt/introduction/ for more information
on how Teleport authentication works.

## Livebook Teams

[Livebook Teams](https://livebook.dev/teams/) users can deploy notebooks with the click of a button with pre-configured Zero Trust Authentication, shared team secrets, and file storages. Both online and airgapped deployment mechanisms are supported.

Furthermore, if you are deploying multi-session apps via [Livebook Teams](https://livebook.dev/teams/), you can programmatically access data from the authenticated user by calling [`Kino.Hub.app_info/0`](https://hexdocs.pm/kino/Kino.Hub.html#app_info/0).

To get started, open up Livebook, click "Add Organization" on the sidebar, and visit the "Airgapped Deployment" section of your organization.
