# Authentication with Cloudflare

Setting up Cloudflare authentication will protect all routes of your notebook. It is particularly useful for adding authentication to deployed notebooks. Cloudflare authentication is provided in addition to [Livebook's authentication](../authentication.md) for authoring notebooks.

Once Cloudflare is enabled, we recommend leaving the "/public" route of your instances still public. This route is used for integration with the [Livebook Badge](https://livebook.dev/badge/) and other conveniences.

## How to

To integrate your Cloudflare Zero Trust authentication with Livebook, set the
`LIVEBOOK_IDENTITY_PROVIDER` environment variable to `cloudflare:<your-team-name>`.

For more details about how to find your `team-name`, see:
https://developers.cloudflare.com/cloudflare-one/glossary/#team-name.

For more information about Cloudflare Zero Trust, see:
https://developers.cloudflare.com/cloudflare-one/.

## Livebook Teams

[Livebook Teams](https://livebook.dev/teams/) users have access to airgapped notebook deployment via Docker, with pre-configured Zero Trust Authentication, shared team secrets, and file storages.

Furthermore, if you are deploying multi-session apps via [Livebook Teams](https://livebook.dev/teams/), you can programmatically access data from the authenticated user by calling [`Kino.Hub.app_info/0`](https://hexdocs.pm/kino/Kino.Hub.html#app_info/0).

To get started, open up Livebook, click "Add Organization" on the sidebar, and visit the "Airgapped Deployment" section of your organization.
